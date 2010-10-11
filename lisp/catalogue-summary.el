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

(require 'database)
(require 'catalogue)
(require 'catalogue-util)


;;; Code:

(defun catalogue-summary-mark-set (arg value)
  "Set mark value on current record or on full disk set depending on arg.
Position is advanced to the next record."
  (unless (db-summary-buffer-p)
    (error "Not in summary buffer"))
  (if arg
      (dbs-in-data-display-buffer
       (catalogue-mapitems
        (lambda ()
          (db-mark-record value))
        (nreverse (catalogue-get-diskset))
        t t t))
    (db-mark-record value))
  (condition-case nil
      (catalogue-next-record)
    (end-of-catalogue nil)))


;;; Interactive commands:

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

(defun catalogue-summary-mark (&optional arg)
  "Mark current record or full disk set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set arg 1)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-unmark (&optional arg)
  "Unmark current record or full disk set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set arg 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))


;;; That's all.

(provide 'catalogue-summary)
