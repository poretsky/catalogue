;;; catalogue-report.el --- Making a short report

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
(require 'catalogue-view)
(require 'catalogue-util)


;;; Code:

(defconst catalogue-report-format "report.fmt"
  "Format file for report making.")


;; Interactive commands:

(defun catalogue-report ()
  "Make a short report of the collection catalogue.
If some records are marked only these ones will be respected.
Each item set in the report is represented by one entry."
  (interactive)
  (with-current-buffer (catalogue-operational-buffer)
    (let ((name nil)
          (category nil))
      (db-hide-records
       (lambda (record)
         (if (and (string= name (db-record-field record 'name dbc-database))
                  (string= category (db-record-field record 'category dbc-database)))
             t
           (setq name (db-record-field record 'name dbc-database)
                 category (db-record-field record 'category dbc-database))
           nil))))
    (let ((resource-path (cons catalogue-resource-directory catalogue-display-format-path))
          (report-format nil))
      (while
          (and resource-path
               (not (file-exists-p
                     (setq report-format
                           (expand-file-name catalogue-report-format
                                             (car resource-path))))))
        (setq resource-path (cdr resource-path)))
      (db-report report-format (catalogue-find-marked-records))))
  (with-current-buffer catalogue-operational-buffer
    (db-unhide-all)
    (dbc-set-hide-p nil))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))


;;; That's all.

(provide 'catalogue-report)
