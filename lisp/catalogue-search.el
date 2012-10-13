;;; catalogue-search.el --- CD/DVD catalogue search engine

;; Copyright (C) 2010  Igor B. Poretsky

;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;; Keywords: database, media collection

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Requirements:

(eval-when-compile
  (require 'cl)
  (require 'database))

(require 'database)
(require 'catalogue-view)
(require 'catalogue-util)


;;; Code:

(defvar catalogue-searchable-fields-list nil
  "Remembers list of searchable fields.")

(defvar catalogue-search-field-history nil
  "Searched field name history.")


;; Utility functions:

(defun catalogue-goto-field (fname)
  "Go to specified field by it's name."
  (db-last-field)
  (while (not (or (eq (catalogue-this-field-name) fname)
                  (zerop (catalogue-this-field-index))))
    (db-previous-field 1)))

(defun catalogue-searchable-fields ()
  "Get list of searchable fields."
  (unless catalogue-searchable-fields-list
    (with-current-buffer (catalogue-operational-buffer)
      (db-next-record 0)
      (db-last-field)
      (while
          (progn
            (push (symbol-name (catalogue-this-field-name)) catalogue-searchable-fields-list)
            (not (zerop (catalogue-this-field-index))))
        (db-previous-field 1))
      (db-view-mode)
      (db-next-record 0)))
  catalogue-searchable-fields-list)

(defun catalogue-search-engine (pattern marked-only &optional field)
  "Search records by specified pattern and return hits list.
In data display buffer jumps to the first hit after the
current item wrapping around the database automatically.
Being called from summary buffer additionally marks all found records
and jumps to the first found hit from the beginning.
If some records are marked and second argument is not `nil',
then only those marked records are searched and the ones unsatisfying
search criteria are unmarked. If field is not specified then general
search is performed by the name and description fields in conjunction."
  (when (catalogue-string-empty-p pattern)
    (error "Empty pattern is not allowed"))
  (let ((amount 0)
        (marked-items (catalogue-find-marked-records))
        (hit-index (catalogue-index))
        (hits nil))
    (with-current-buffer (catalogue-operational-buffer)
      (db-unmark-all)
      (if (null field)
          (let ((original-position (catalogue-index))
                (first-hit))
            (catalogue-goto-field 'name)
            (db-search-field pattern t)
            (setq first-hit (catalogue-index))
            (db-jump-to-record original-position)
            (catalogue-goto-field 'description)
            (db-search-field pattern t)
            (if (= original-position (catalogue-index))
                (unless (= (catalogue-index) first-hit)
                  (db-jump-to-record first-hit))
              (unless (= original-position first-hit)
                (when (or (and (< original-position first-hit)
                               (< first-hit (catalogue-index)))
                          (and (< original-position first-hit)
                               (< (catalogue-index) original-position))
                          (and (< (catalogue-index) original-position)
                               (< first-hit (catalogue-index))))
                  (db-jump-to-record first-hit)))))
        (db-first-field)
        (do ((fields (catalogue-searchable-fields) (cdr fields)))
            ((or (null fields)
                 (string= field (car fields))))
          (db-next-field 1))
        (db-search-field pattern t))
      (and (setq hits (catalogue-find-marked-records))
           (setq hit-index (catalogue-index))
           (db-unmark-all)))
    (when (and (db-summary-buffer-p) marked-only hits marked-items)
      (let ((filtered nil))
        (mapc
         (lambda (item)
           (when (member item marked-items)
             (push item filtered)))
         hits)
        (setq hits filtered)))
    (when (db-summary-buffer-p)
      (setq hit-index (car-safe hits))
      (if hit-index
          (mapc
           (lambda (item)
             (when (< item hit-index)
               (setq hit-index item)))
           hits)
        (setq hit-index dbs-index)))
    (when hits
      (setq amount (length hits))
      (when (db-summary-buffer-p)
        (if (or marked-only (not marked-items))
            (setq marked-items hits)
          (mapc
           (lambda (item)
             (add-to-list 'marked-items item))
           hits))))
    (when marked-items
      (db-in-data-display-buffer
        (mapc
         (lambda (item)
           (db-select-record item)
           (db-mark-record 1))
         marked-items)))
    (db-jump-to-record hit-index)
    (when (db-data-display-buffer-p)
      (catalogue-summary-synch-position))
    (message "%d item%s found" amount (if (= 1 amount) "" "s"))
    hits))


;; Interactive commands:

(defun catalogue-search (pattern &optional arg)
  "Search records by specified pattern examining name and description fields.
In data display buffer jumps to the first hit after the
current item wrapping around the database automatically.
Being called from summary buffer additionally marks all found records
and jumps to the first found hit from the beginning.
If some records are marked and prefix argument is in effect,
then only those marked records are searched and the ones
unsatisfying search criteria are unmarked."
  (interactive
   (list
    (read-string "Enter search pattern: ")
    current-prefix-arg))
  (let ((hits (catalogue-search-engine pattern arg)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon
       (if hits
           'search-hit
         'search-miss))
      (when hits
        (if (db-summary-buffer-p)
            (emacspeak-speak-line)
          (emacspeak-speak-current-window))))))

(defun catalogue-search-by-field (field pattern &optional arg)
  "Search records by specified field and pattern.
In data display buffer jumps to the first hit after the
current item wrapping around the database automatically.
Being called from summary buffer additionally marks all found records
and jumps to the first found hit from the beginning.
If some records are marked and prefix argument is in effect,
then only those marked records are searched and the ones
unsatisfying search criteria are unmarked."
  (interactive
   (list
    (completing-read "Choose field to search by: "
                     (catalogue-searchable-fields)
                     nil t nil
                     'catalogue-search-field-history
                     (car (catalogue-searchable-fields)))
    (read-string "Enter search pattern: ")
    current-prefix-arg))
  (let ((hits (catalogue-search-engine pattern arg field)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon
       (if hits
           'search-hit
         'search-miss))
      (when hits
        (if (db-summary-buffer-p)
            (emacspeak-speak-line)
          (emacspeak-speak-current-window))))))


;;; That's all.

(provide 'catalogue-search)
