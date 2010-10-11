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
(require 'catalogue-util)


;;; Code:

;;; Interactive commands:

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
  (setq catalogue-editing-p nil)
  (db-sort t)
  (db-save-database)
  (db-view-mode)
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
  (dbf-set-this-field-modified-p nil)
  (dbf-set-this-record-modified-p nil)
  (dbc-set-database-modified-p nil)
  (let ((index (catalogue-index))
        (marked (catalogue-find-marked-records)))
    (db-exit t)
    (catalogue-view)
    (mapcar
     (lambda (item)
       (db-jump-to-record item)
       (db-mark-record 1))
     marked)
    (db-jump-to-record (min (database-no-of-records dbc-database) index)))
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
  "Go to the next field or insert a new line in the multiline description."
  (interactive)
  (if (eq (dbf-this-field-name) 'description)
      (call-interactively 'db-newline)
    (call-interactively 'catalogue-next-line-or-field)))


;;; That's all.

(provide 'catalogue-edit)
