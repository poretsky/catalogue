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

(defvar catalogue-operational-buffer nil
  "Invisible buffer for internal use.")


(defun catalogue-synchronize-with (orig-buffer)
  "Synchronize current data display buffer with specified original."
  (db-copy-buffer-local-variables orig-buffer)
  (setq dbf-this-record (make-record dbc-database))
  (db-emergency-restore-format t))

(defun catalogue-operational-buffer ()
  "Get operational buffer synchronized with current
data display buffer creating it if necessary."
  (db-in-data-display-buffer
   (dbf-process-current-record-maybe t)
   (let ((orig-buffer (current-buffer))
         (database dbc-database))
     (unless (buffer-live-p catalogue-operational-buffer)
       (setq catalogue-operational-buffer (db-make-data-display-buffer database nil))
       (database-set-data-display-buffers
        database
        (cons catalogue-operational-buffer (database-data-display-buffers database)))
       (with-current-buffer catalogue-operational-buffer
         (rename-buffer " *Catalogue workspace* " t)
         (setq catalogue-operational-buffer-name (buffer-name))))
     (with-current-buffer catalogue-operational-buffer
       (catalogue-synchronize-with orig-buffer))
     catalogue-operational-buffer)))

(defun catalogue-delete (&optional items)
  "Delete record or clear it if it is the only one.
If optional argument is specified it is treated as a list of record indexes
to be deleted in the descending order. In this case all the work
will be done in the operational buffer that will then be properly
synchronized with current data display buffer."
  (if items
      (let ((original-index dbc-index))
        (with-current-buffer (catalogue-operational-buffer)
          (mapcar
           (lambda (item)
             (db-jump-to-record item)
             (catalogue-delete)
             (when (< item original-index)
               (setq original-index (1- original-index))))
           items)
          (db-jump-to-record (min (max 1 original-index) (database-no-of-records dbc-database))))
        (catalogue-synchronize-with catalogue-operational-buffer))
    (when (= 1 (database-no-of-records dbc-database))
      (db-add-record)
      (dbf-set-this-record-modified-p t)
      (dbf-displayed-record-set-field 'id "")
      (db-view-mode)
      (db-next-record 1))
    (db-delete-record t)))

(defun catalogue-mapitems (action items)
  "Apply specified action to the listed items. The first argument
should be a function that works on the current record and returns
`t' in the case of success or `nil' otherwise. The second argument
should contain a list of record indexes to be processed."
  (unless (db-data-display-buffer-p)
    (error "Not in data display buffer"))
  (let ((processed 0)
        (to-process (length items)))
    (with-current-buffer (catalogue-operational-buffer)
      (mapcar
       (lambda (item)
         (db-jump-to-record item)
         (when (funcall action)
           (setq processed (1+ processed))))
       items)
      (unless (zerop processed)
        (db-save-database)))
    (db-next-record 0)
    (message "%d of %d items processed" processed to-process)))

(defun catalogue-links-gather (link-predicate)
  "Apply specified predicate to each record link and return
list of indexes of satisfying ones in reverse order."
  (let ((gathered nil))
    (maplinks
     (lambda (link)
       (when (funcall link-predicate link)
         (setq gathered (cons maplinks-index gathered))))
     dbc-database)
    gathered))

(defun catalogue-records-gather (record-predicate)
  "Apply specified predicate to each record and return
list of indexes of satisfying ones in reverse order."
  (catalogue-links-gather
   (lambda (link)
     (funcall record-predicate (link-record link)))))

(defun catalogue-list-by (field content &optional unmatched)
  "Get list of record indexes where specified field is matched
or unmatched to given content depending on the third optional argument.
Matching is done by `string='. Returned list is in the reverse order."
  (catalogue-records-gather
   (lambda (record)
     (if unmatched
         (not (string= content (record-field record field dbc-database)))
       (string= content (record-field record field dbc-database))))))

(defun catalogue-list-the-same (field)
  "Get list of record indexes with the same specified field content
as the current one. Returned list is in the reverse order."
  (catalogue-list-by field (dbf-displayed-record-field field)))

(defun catalogue-get-diskset ()
  "Get list of record indexes for the diskset
which displayed record belongs to. Returned list is in the reverse order."
  (let ((name (dbf-displayed-record-field 'name))
        (category (dbf-displayed-record-field 'category)))
    (catalogue-records-gather
     (lambda (record)
       (and (string= name (record-field record 'name dbc-database))
            (string= category (record-field record 'category dbc-database)))))))

(defun catalogue-find-marked-records ()
  "Get list of marked records indexes. Returned list is in the reverse order."
  (catalogue-links-gather 'link-markedp))


;;; That's all.

(provide 'catalogue-util)
