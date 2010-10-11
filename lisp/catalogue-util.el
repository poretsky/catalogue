;;; catalogue-util.el --- Catalogue utility functions used by other modules

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


;;; Code:

(defvar catalogue-restore-summary nil
  "This variable is set when summary buffer is temporary killed.
Non-nil value denotes that summary should be restored afterwards.
When it contains `t' the summary window becomes active.")


(defun catalogue-delete-record ()
  "Delete record or clear it if it is the only one."
  (when (= 1 (database-no-of-records dbc-database))
    (db-add-record)
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'id "")
    (db-view-mode)
    (db-next-record 1))
  (db-delete-record t))

(defun catalogue-mapitems (action items &optional quiet unpos unsafe)
  "Apply specified action to the listed items. The first argument
should be a function that works on the current record and returns
`t' in the case of success or `nil' otherwise. The second argument
should contain a list of record indexes to be processed.
The third optional argument disables typing of the result message.
The fourth optional argument disables restoring cursor position.
The fifth optional argument disables saving the database."
  (unless (db-data-display-buffer-p)
    (error "Not in data display buffer"))
  (let ((processed 0)
        (to-process (length items))
        (original-index dbc-index))
    (mapcar
     (lambda (item)
       (db-jump-to-record item)
       (when (funcall action)
         (setq processed (1+ processed))))
     items)
    (unless (or unsafe (zerop processed))
      (db-save-database))
    (unless unpos
      (db-jump-to-record original-index))
    (catalogue-summary-synch-position)
    (unless quiet
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))
      (message "%d of %d items processed" processed to-process))))

(defun catalogue-list-by (field content &optional unmatched)
  "Get list of record indexes where specified field is matched
or unmatched to given content depending on the third optional argument.
Matching is done by `string='. Returned list is in the reverse order."
  (let ((items nil))
    (maplinks
     (lambda (link)
       (if unmatched
           (unless (string= content (record-field (link-record link) field dbc-database))
             (setq items (cons maplinks-index items)))
         (when (string= content (record-field (link-record link) field dbc-database))
           (setq items (cons maplinks-index items)))))
     dbc-database)
    items))

(defun catalogue-list-the-same (field)
  "Get list of record indexes with the same specified field content
as the current one. Returned list is in the reverse order."
  (catalogue-list-by field (dbf-displayed-record-field field)))

(defun catalogue-get-diskset ()
  "Get list of record indexes for the diskset
which displayed record belongs to. Returned list is in the reverse order."
  (catalogue-list-the-same 'name))

(defun catalogue-find-marked-records ()
  "Get list of marked records indexes. Returned list is in the reverse order."
  (let ((marked nil))
    (maplinks
     (lambda (link)
       (when (link-markedp link)
         (setq marked (cons maplinks-index marked))))
     dbc-database)
    marked))


;;; That's all.

(provide 'catalogue-util)
