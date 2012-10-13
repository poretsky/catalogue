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

(eval-when-compile
  (require 'cl)
  (require 'database))

(require 'database)
(require 'catalogue)


;;; Code:

(defvar catalogue-restore-summary nil
  "This variable is set when summary buffer is temporary killed.
Non-nil value denotes that summary should be restored afterwards.
When it contains `t' the summary window becomes active.")

(defvar catalogue-operational-buffer nil
  "Invisible buffer for internal use.")


(defun catalogue-record-init ()
  "Return null record initializer."
  '(:alist
    (id . "")
    (name . "")
    (category . "")
    (set . 0)
    (unit . 0)
    (media . "")
    (owner . nil)
    (borrower . nil)
    (since . nil)
    (place . "")
    (description . "")))

(defun catalogue-index ()
  "Retrieve current record index in the database."
  (edb--S :index))

(defun catalogue-record-by-index (index)
  "Get record by it's index."
  (aref (edb-tag :vov dbc-database) (1- index)))

(defun catalogue-this-field-name ()
  "Return current field name as symbol."
  (dbf-this-field-name (edb--S :this-ds)))

(defun catalogue-this-field-index ()
  "Get current field index in the displayspec."
  (edb--S :this-fidx))

(defun catalogue-marked-p (record)
  "Yield true if specified record is marked."
  (edb-tagp (edb-tag :markedp dbc-database) record))

(defun catalogue-summary-buffer ()
  "Return summary buffer if it exists or nil otherwise."
  (let ((buf (edb--S :sumbuf)))
    (and (bufferp buf)
         (buffer-name buf)
         buf)))

(defun catalogue-kill-summary ()
  "Kill summary buffer if it exists."
  (let ((buf (catalogue-summary-buffer)))
    (when buf
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun catalogue-synchronize-with (orig-buffer)
  "Synchronize current data display buffer with specified original."
  (db-copy-buffer-local-variables orig-buffer)
  (setq dbf-this-record (db-make-record dbc-database (catalogue-record-init)))
  (db-emergency-restore-format t))

(defun catalogue-operational-buffer ()
  "Get operational buffer synchronized with current
data display buffer creating it if necessary."
  (declare (special catalogue-operational-buffer-name))
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (let ((orig-buffer (current-buffer))
          (database dbc-database))
      (unless (buffer-live-p catalogue-operational-buffer)
        (setq catalogue-operational-buffer (db-make-data-display-buffer database nil))
        (with-current-buffer catalogue-operational-buffer
          (rename-buffer " *Catalogue workspace* " t)
          (setq catalogue-operational-buffer-name (buffer-name))))
      (with-current-buffer catalogue-operational-buffer
        (catalogue-synchronize-with orig-buffer))
      catalogue-operational-buffer)))

(defun catalogue-delete (&optional items)
  "Delete current record or clear it if it is the only one.
If optional argument is specified it is treated as a list of record indexes
to be deleted in the descending order. In this case all the work
will be done in the operational buffer that will then be properly
synchronized with current data display buffer."
  (if items
      (let ((original-index (catalogue-index)))
        (with-current-buffer (catalogue-operational-buffer)
          (mapc
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
      (mapc
       (lambda (item)
         (db-jump-to-record item)
         (when (funcall action)
           (setq processed (1+ processed))))
       items)
      (unless (zerop processed)
        (db-save-database)))
    (db-next-record 0)
    (message "%d of %d items processed" processed to-process)))

(defun catalogue-records-gather (predicate)
  "Apply specified predicate to each record and return
list of indexes of satisfying ones in reverse order."
  (let ((gathered nil))
    (db-maprecords
     (lambda (record)
       (when (funcall predicate record)
         (push db-lmap-index gathered))))
    gathered))

(defun catalogue-list-by (field content &optional unmatched)
  "Get list of record indexes where specified field is matched
or unmatched to given content depending on the third optional argument.
Matching is done by `string='. Returned list is in the reverse order."
  (catalogue-records-gather
   (lambda (record)
     (if unmatched
         (not (string= content (db-record-field record field dbc-database)))
       (string= content (db-record-field record field dbc-database))))))

(defun catalogue-list-the-same (field)
  "Get list of record indexes with the same specified field content
as the current one. Returned list is in the reverse order."
  (catalogue-list-by field (dbf-displayed-record-field field)))

(defun catalogue-list-item-set ()
  "Get list of record indexes for the item set
which displayed record belongs to. Returned list is in the reverse order."
  (let ((name (dbf-displayed-record-field 'name))
        (category (dbf-displayed-record-field 'category)))
    (catalogue-records-gather
     (lambda (record)
       (and (string= name (db-record-field record 'name dbc-database))
            (string= category (db-record-field record 'category dbc-database)))))))

(defun catalogue-find-marked-records ()
  "Get list of marked records indexes. Returned list is in the reverse order."
  (catalogue-records-gather 'catalogue-marked-p))


;;; That's all.

(provide 'catalogue-util)
