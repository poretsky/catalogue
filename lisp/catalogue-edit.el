;;; catalogue-edit.el --- Catalogue editing capabilities

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

(require 'database)
(require 'catalogue)
(require 'catalogue-strings)
(require 'catalogue-util)


;;; Code:

;; Separate history for minibuffer inputs:

(defvar catalogue-edit-name-history nil
  "Minibuffer history for name input.")

(defvar catalogue-edit-category-history nil
  "Minibuffer history for input category.")

(defvar catalogue-edit-media-type-history nil
  "Minibuffer history for input media type.")

(defvar catalogue-edit-owner-history nil
  "Minibuffer history for input owner.")

(defvar catalogue-edit-place-history nil
  "Minibuffer history for input place.")


;; Utility functions:

(defun catalogue-edit-string-input (field history)
  "Input specified field content using history."
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field-and-redisplay
   field
   (read-string
    "Input new value or try history: "
    nil history
    (dbf-displayed-record-field field)
    t)))

(defun catalogue-edit-completing-input (field collection history)
  "Input specified field content using history and completions."
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field-and-redisplay
   field
   (completing-read
    "Input with completions or try history: "
    collection nil nil nil history
    (dbf-displayed-record-field field)
    t)))

(defun catalogue-known-field-values (field predefined)
  "Get list of known values for specified field.
The second argument provides alist of predefined values."
  (let ((collection (mapcar 'cdr predefined)))
    (maprecords
     (lambda (record)
       (unless (= maplinks-index dbc-index)
         (let ((item (record-field record field dbc-database)))
           (and item
                (not (string= "" item))
                (add-to-list 'collection item)))))
     dbc-database)
    collection))


;; Interactive commands:

(defun catalogue-edit ()
  "Edit current catalogue record."
  (interactive)
  (unless (or (db-data-display-buffer-p) (db-summary-buffer-p))
    (error "Not in data display or summary buffer"))
  (when (and (not catalogue-unknown-disk)
             (catalogue-empty-p))
    (error "Catalogue is empty"))
  (if (db-summary-buffer-p)
      (progn
        (dbs-exit)
        (setq catalogue-restore-summary t))
    (setq catalogue-restore-summary (dbf-summary-buffer))
    (dbf-kill-summary))
  (setq catalogue-affected-set nil)
  (setq catalogue-editing-p t)
  (db-next-record 0)
  (db-first-field)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defun catalogue-commit ()
  "Commit current record to the database after editing."
  (interactive)
  (unless (db-data-display-buffer-p)
    (error "Not in data display buffer"))
  (unless (eq dbf-minor-mode 'edit)
    (error "Not in editing mode"))
  (db-commit-record)
  (setq catalogue-editing-p nil)
  (db-view-mode)
  (when catalogue-affected-set
    (with-current-buffer (catalogue-operational-buffer)
      (let ((original-index dbc-index))
        (mapcar
         (lambda (item)
           (db-jump-to-record (car item))
           (unless (= (dbf-displayed-record-field 'set) (cdr item))
             (dbf-set-this-record-modified-p t)
             (dbf-displayed-record-set-field 'set (cdr item))))
         catalogue-affected-set)
        (db-jump-to-record original-index)))
    (catalogue-synchronize-with catalogue-operational-buffer)
    (setq catalogue-affected-set nil))
  (db-sort t)
  (db-save-database)
  (db-next-record 0)
  (cond
   ((eq catalogue-restore-summary t)
    (setq catalogue-restore-summary nil)
    (db-summary))
   (catalogue-restore-summary
    (setq catalogue-restore-summary nil)
    (save-selected-window
      (db-summary)))
   (t nil))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun catalogue-cancel ()
  "Finish editing without saving changes."
  (interactive)
  (unless (db-data-display-buffer-p)
    (error "Not in data display buffer"))
  (unless (eq dbf-minor-mode 'edit)
    (error "Not in editing mode"))
  (setq catalogue-affected-set nil)
  (dbf-set-this-field-modified-p nil)
  (dbf-set-this-record-modified-p nil)
  (dbc-set-database-modified-p nil)
  (let ((index (catalogue-index))
        (marked (catalogue-find-marked-records)))
    (db-exit t)
    (catalogue-view)
    (db-jump-to-record (min (database-no-of-records dbc-database) index))
    (when marked
      (mapcar
       (lambda (item)
         (db-select-record item)
         (db-mark-record 1))
       marked)
      (db-jump-to-record (min (database-no-of-records dbc-database) index))))
  (cond
   ((eq catalogue-restore-summary t)
    (setq catalogue-restore-summary nil)
    (db-summary))
   (catalogue-restore-summary
    (setq catalogue-restore-summary nil)
    (save-selected-window
      (db-summary)))
   (t nil))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun catalogue-next-line-or-field ()
  "Go to the next line or field in editing mode wrapping around the record if enabled."
  (interactive)
  (if (or catalogue-record-wraparound
          (> (count-lines (point) (point-max)) 1))
      (let ((prev dbf-this-field-index))
        (db-next-line-or-field 1)
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (unless (= prev dbf-this-field-index)
            (emacspeak-auditory-icon 'select-object))
          (emacspeak-speak-line)))
    (signal 'end-of-buffer nil)))

(defun catalogue-previous-line-or-field ()
  "Go to the previous line or field in editing mode wrapping around the record if enabled."
  (interactive)
  (if (or catalogue-record-wraparound
          (> (count-lines (point-min) (point)) 1))
      (let ((prev dbf-this-field-index))
        (db-previous-line-or-field 1)
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (unless (= prev dbf-this-field-index)
            (emacspeak-auditory-icon 'select-object))
          (emacspeak-speak-line)))
    (signal 'beginning-of-buffer nil)))

(defun catalogue-newline ()
  "Input field content with history and completions if available
or insert a new line in the multiline description."
  (interactive)
  (let ((field (dbf-this-field-name)))
    (cond
     ((eq field 'name)
      (catalogue-edit-string-input 'name catalogue-edit-name-history))
     ((eq field 'category)
      (catalogue-edit-completing-input
       'category
       (catalogue-known-field-values
        'category
        (cdr (assoc (catalogue-language)
                    catalogue-category-names-alist)))
       catalogue-edit-category-history))
     ((eq field 'media)
      (catalogue-edit-completing-input
       'media
       (catalogue-known-field-values 'media catalogue-media-types-alist)
       catalogue-edit-media-type-history))
     ((eq field 'owner)
      (catalogue-edit-string-input 'owner catalogue-edit-owner-history))
     ((eq field 'place)
      (catalogue-edit-string-input 'place catalogue-edit-place-history))
     ((eq field 'description)
      (call-interactively 'db-newline))
     (t
      (error "Function unavailable in this field"))))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))


;;; That's all.

(provide 'catalogue-edit)
