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

(eval-when-compile (require 'cl))
(require 'database)
(require 'catalogue)
(require 'catalogue-util)


;;; Code:

(defvar catalogue-searchable-fields-list nil
  "Remembers list of searchable fields.")

(defvar catalogue-search-field-history nil
  "Searched field name history.")


(defun catalogue-searchable-fields ()
  "Get list of searchable fields."
  (unless catalogue-searchable-fields-list
    (with-current-buffer (catalogue-operational-buffer)
      (db-next-record 0)
      (db-last-field)
      (setq catalogue-searchable-fields-list
            (do ((result (list (symbol-name (dbf-this-field-name)))
                         (cons (symbol-name (dbf-this-field-name)) result)))
                ((zerop dbf-this-field-index)
                 result)
              (db-previous-field 1)))
      (db-view-mode)
      (db-next-record 0)))
  catalogue-searchable-fields-list)


;;; Interactive commands:

(defun catalogue-search (field pattern)
  "Search record by specified field and pattern.
Being called from summary buffer additionally marks all found records."
  (interactive
   (list
    (completing-read "Choose field to search by: "
                     (catalogue-searchable-fields)
                     nil t nil
                     'catalogue-search-field-history
                     (car (catalogue-searchable-fields)))
    (read-string "Enter search pattern: ")))
  (when (string= pattern "")
    (error "Empty pattern is not allowed"))
  (let ((amount 0)
        (hits)
        (hit-index))
    (with-current-buffer (catalogue-operational-buffer)
      (db-first-field)
      (do ((fields (catalogue-searchable-fields) (cdr fields)))
          ((or (null fields)
               (string= field (car fields))))
        (db-next-field 1))
      (db-unmark-all)
      (db-search-field pattern t)
      (and (setq hits (catalogue-find-marked-records))
           (setq hit-index dbc-index)
           (db-unmark-all)))
    (when hits
      (setq amount (length hits))
      (when (db-summary-buffer-p)
        (dbs-in-data-display-buffer
         (mapcar
          (lambda (item)
            (db-select-record item)
            (db-mark-record 1))
          hits)))
      (db-jump-to-record hit-index)
      (when (db-data-display-buffer-p)
        (catalogue-summary-synch-position)))
    (message "%d item%s found" amount (if (= 1 amount) "" "s"))
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
