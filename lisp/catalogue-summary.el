;;; catalogue-summary.el --- Catalogue summary mode specific features

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

(require 'easymenu)
(require 'database)
(require 'catalogue-view)
(require 'catalogue-util)


;;; Code:

(defun catalogue-summary-mark-set (arg value)
  "Set mark value on current record or on full item set
depending on arg. If arg is not `nil' and not `set' it is treated
as name of field by which record should be selected.
At the end of operation position is advanced to the next
available record after the last processed one."
  (unless (db-summary-buffer-p)
    (error "Not in summary buffer"))
  (if (null arg)
      (db-mark-record value)
    (dbs-in-data-display-buffer
     (mapcar
      (lambda (item)
        (db-select-record item)
        (db-mark-record value))
      (nreverse
       (if (eq arg 'set)
           (catalogue-list-item-set)
         (catalogue-list-the-same arg)))))
    (dbs-move-to-proper-record))
  (condition-case nil
      (catalogue-next-record)
    (end-of-catalogue nil)))


;;; Interactive commands:

;; Entry point:

(defun catalogue-summary ()
  "Pop up summary window if database is not empty
or synchronize summary window when called from there."
  (interactive)
  (if (db-summary-buffer-p)
      (progn
        (db-summary)
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (emacspeak-auditory-icon 'select-object)
          (emacspeak-speak-line)))
    (unless (db-data-display-buffer-p)
      (error "Not in data display buffer"))
    (when (catalogue-empty-p)
      (error "Catalogue is empty"))
    (db-summary)
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-line))))


;; Marking:

(defun catalogue-summary-mark (&optional arg)
  "Mark current record or full item set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set (and arg 'set) 1)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-unmark (&optional arg)
  "Unmark current record or full item set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set (and arg 'set) 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-mark-category ()
  "Mark all records of category the current one belongs to.
Position is advanced to the next record."
  (interactive)
  (catalogue-summary-mark-set 'category 1)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-unmark-category ()
  "Unmark all records of category the current one belongs to.
Position is advanced to the next record."
  (interactive)
  (catalogue-summary-mark-set 'category 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))


;; Navigation:

(put 'no-more-marks 'error-conditions '(error no-more-marks))
(put 'no-more-marks 'error-message "No more marks")

(defun catalogue-summary-next-record (&optional arg)
  "Go to the next record in summary buffer.
with prefix argument go to the next item set."
  (interactive "P")
  (catalogue-next-record arg)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-next-mark ()
  "Go to the next marked record."
  (interactive)
  (let ((marks (catalogue-find-marked-records)))
    (if (and marks
             (or catalogue-database-wraparound
                 (< (catalogue-index) (car marks))))
        (db-next-marked-record 1)
      (signal 'no-more-marks nil)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-previous-record (&optional arg)
  "Go to the previous record in summary buffer.
With prefix argument go to the previous item set."
  (interactive "P")
  (catalogue-previous-record arg)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-previous-mark ()
  "Go to the previous marked record."
  (interactive)
  (let ((marks (catalogue-find-marked-records)))
    (if (and marks
             (or catalogue-database-wraparound
                 (> (catalogue-index) (car (nreverse marks)))))
        (db-previous-marked-record 1)
      (signal 'no-more-marks nil)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-first-record ()
  "Go to the first record in summary buffer."
  (interactive)
  (dbs-in-data-display-buffer
   (db-first-record)
   (catalogue-summary-synch-position))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-last-record ()
  "Go to the last record in summary buffer."
  (interactive)
  (dbs-in-data-display-buffer
   (db-last-record)
   (catalogue-summary-synch-position))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-scroll-up ()
  "Scroll up in summary buffer."
  (interactive)
  (dbs-scroll-up)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-scroll-down ()
  "Scroll down in summary buffer."
  (interactive)
  (dbs-scroll-down)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))


;; Key bindings for summary view:

(defvar catalogue-summary-map (make-keymap)
  "catalogue summary mode keymap.")
(suppress-keymap catalogue-summary-map t)
(loop for binding in
      '(([down] . catalogue-summary-next-record)
        ("n" . catalogue-summary-next-record)
        ([up] . catalogue-summary-previous-record)
        ("p" . catalogue-summary-previous-record)
        ([C-down] . catalogue-summary-next-mark)
        ("\C-n" . catalogue-summary-next-mark)
        ([C-up] . catalogue-summary-previous-mark)
        ("\C-p" . catalogue-summary-previous-mark)
        ([next] . catalogue-summary-scroll-up)
        ([prior] . catalogue-summary-scroll-down)
        ([C-home] . catalogue-summary-first-record)
        ([C-end] . catalogue-summary-last-record)
        ("\C-s" . db-isearch-forward)
        ("\C-r" . db-isearch-backward)
        ("s" . catalogue-search)
        ("g" . catalogue-summary)
        ([return] . catalogue-edit)
        ("e" . catalogue-edit)
        ("/" . catalogue-disk-identify)
        ("I" . catalogue-disk-identify)
        ("B" . catalogue-borrow)
        ("L" . catalogue-lend)
        ("R" . catalogue-release)
        ("G" . catalogue-give-up)
        ("\C-cr" . catalogue-reassign)
        ("m" . catalogue-summary-mark)
        ("u" . catalogue-summary-unmark)
        ("\C-cm" . catalogue-summary-mark-category)
        ("\C-cu" . catalogue-summary-unmark-category)
        ("\M-u" . db-unmark-all)
        ("\C-d" . catalogue-unregister)
        ("?" . describe-mode)
        ("q" . dbs-exit))
      do
      (define-key catalogue-summary-map (car binding) (cdr binding)))


;;; That's all.

(provide 'catalogue-summary)
