;;; catalogue-commands.el --- Some additional media catalogue control commands

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

(defconst catalogue-date-format "%B %e, %Y"
  "Date display format.")


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
    (unless (and unsafe (not (zerop processed)))
      (db-save-database))
    (unless unpos
      (db-jump-to-record original-index))
    (unless quiet
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))
      (message "%d of %d items processed" processed to-process))))

(defun catalogue-get-diskset ()
  "Get list of record indexes for the diskset
which displayed record belongs to."
  (let ((name (dbf-displayed-record-field 'name))
        (items nil))
    (maplinks
     (lambda (link)
       (when (string= name (record-field (link-record link) 'name dbc-database))
         (setq items (cons maplinks-index items))))
     dbc-database)
    items))


;;; Interactive commands:

(defun catalogue-borrow (&optional entire)
  "Register this disk as borrowed. When called with prefix argument
in data display buffer the action is applied to the entire disk set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (cond
   (entire
    (catalogue-mapitems 'catalogue-borrow (catalogue-get-diskset)))
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-borrow items)
         (call-interactively 'catalogue-borrow)))))
   (t
    (if (catalogue-native-p)
        (if (interactive-p)
            (error "This disk is native, so cannot be borrowed")
          nil)
      (if (catalogue-borrowed-p)
          (if (interactive-p)
              (error "This disk is already borrowed")
            nil)
        (dbf-set-this-record-modified-p t)
        (dbf-displayed-record-set-field-and-redisplay
         'since
         (format-time-string
          catalogue-date-format))
        (if (not (interactive-p))
            t
          (db-save-database)
          (db-next-record 0)
          (when (featurep 'emacspeak)
            (emacspeak-auditory-icon 'save-object))))))))

(defun catalogue-lend (lender &optional entire)
  "Register this disk as lended. When called with prefix argument
in data display buffer the action is applied to the entire disk set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "sLender: \nP")
  (cond
   (entire
    (catalogue-mapitems 'catalogue-lend (catalogue-get-diskset)))
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-lend items)
         (call-interactively 'catalogue-lend)))))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field
     'since
     (format-time-string catalogue-date-format))
    (dbf-displayed-record-set-field-and-redisplay 'lended lender)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))))))

(defun catalogue-release (&optional entire)
  "Release borrowed or lended item. When called with prefix argument
in data display buffer the action is applied to the entire disk set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (cond
   (entire
    (catalogue-mapitems 'catalogue-release (catalogue-get-diskset)))
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-release items)
         (call-interactively 'catalogue-release)))))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'lended nil)
    (dbf-displayed-record-set-field-and-redisplay 'since nil)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))))))

(defun catalogue-give-up (new-owner &optional entire)
  "Register this disk as alien. When called with prefix argument
in data display buffer the action is applied to the entire disk set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "sNew owner: \nP")
  (cond
   (entire
    (catalogue-mapitems 'catalogue-give-up (catalogue-get-diskset)))
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-give-up items)
         (call-interactively 'catalogue-give-up)))))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'since nil)
    (dbf-displayed-record-set-field-and-redisplay 'owner new-owner)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))))))

(defun catalogue-unregister (&optional entire)
  "Forget this disk forever. When called with prefix argument
in data display buffer the action is applied to the entire disk set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (let ((items nil)
        (processed 0))
    (cond
     (entire
      (when (or (not (interactive-p))
                (y-or-n-p "Forget this entire disk set forever? "))
        (setq items (catalogue-get-diskset)
              processed (length items))
        (catalogue-mapitems 'catalogue-delete-record items t t)))
     ((db-summary-buffer-p)
      (dbs-in-data-display-buffer
       (if (setq items (catalogue-find-marked-records))
           (when (or (not (interactive-p))
                     (y-or-n-p "Forget all marked items forever? "))
             (setq processed (length items))
             (let ((orig dbc-index))
               (catalogue-mapitems
                (lambda ()
                  (when (< dbc-index orig)
                    (setq orig (1- orig)))
                  (catalogue-delete-record))
                items t t)
               (db-jump-to-record (max 1 orig))))
         (when (or (not (interactive-p))
                   (y-or-n-p "Forget this disk forever? "))
           (catalogue-delete-record)
           (db-save-database)
           (setq processed 1)))))
     (t
      (when (or (not (interactive-p))
                (y-or-n-p "Forget this disk forever? "))
        (catalogue-delete-record)
        (db-save-database)
        (setq processed 1))))
    (unless (zerop processed)
      (if (catalogue-empty-p)
          (dbf-kill-summary)
        (dbf-fill-summary-buffer-and-move-to-proper-record))
      (when (interactive-p)
        (when (featurep 'emacspeak)
          (emacspeak-auditory-icon 'delete-object))
        (message "%d deletion%s done" processed (if (> processed 1) "s" ""))))))


;;; That's all.

(provide 'catalogue-commands)
