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

(eval-when-compile
  (require 'database)
  (require 'db-summary))

(require 'database)
(require 'catalogue)
(require 'catalogue-util)


;;; Code:

(defconst catalogue-date-format "%B %e, %Y"
  "Date display format.")


;;; Interactive commands:

(defun catalogue-borrow (&optional entire)
  "Register this item as borrowed. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (cond
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-borrow items)
         (call-interactively 'catalogue-borrow)))))
   (entire
    (catalogue-mapitems 'catalogue-borrow (catalogue-list-item-set)))
   (t
    (if (catalogue-native-p)
        (if (interactive-p)
            (error "This item is native, so cannot be borrowed")
          nil)
      (if (catalogue-borrowed-p)
          (if (interactive-p)
              (error "This item is already borrowed")
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

(defun catalogue-lend (borrower &optional entire)
  "Register this item as lended. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "sBorrower: \nP")
  (cond
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems
            (lambda ()
              (catalogue-lend borrower))
            items)
         (catalogue-lend borrower)
         (db-save-database)
         (db-next-record 0)))))
   (entire
    (catalogue-mapitems
     (lambda ()
       (catalogue-lend borrower))
     (catalogue-list-item-set)))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field
     'since
     (format-time-string catalogue-date-format))
    (dbf-displayed-record-set-field-and-redisplay 'borrower borrower)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0))))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'save-object)))

(defun catalogue-release (&optional entire)
  "Release borrowed or lended item. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (cond
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-release items)
         (call-interactively 'catalogue-release)))))
   (entire
    (catalogue-mapitems 'catalogue-release (catalogue-list-item-set)))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'borrower nil)
    (dbf-displayed-record-set-field-and-redisplay 'since nil)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))))))

(defun catalogue-acquire (&optional entire)
  "Make this item native. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (cond
   ((db-summary-buffer-p)
    (dbs-in-data-display-buffer
     (let ((items (catalogue-find-marked-records)))
       (if items
           (catalogue-mapitems 'catalogue-acquire items)
         (call-interactively 'catalogue-acquire)))))
   (entire
    (catalogue-mapitems 'catalogue-acquire (catalogue-list-item-set)))
   (t
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'owner nil)
    (if (not (interactive-p))
        t
      (db-save-database)
      (db-next-record 0)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))))))

(defun catalogue-give-up (new-owner &optional entire)
  "Register this item as alien. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "sNew owner: \nP")
  (if (db-summary-buffer-p)
      (dbs-in-data-display-buffer
       (let ((items (catalogue-find-marked-records)))
         (if items
             (catalogue-mapitems
              (lambda ()
                (catalogue-give-up new-owner))
              items)
           (unless (catalogue-give-up new-owner)
             (error "This is not native item"))
           (db-save-database)
           (db-next-record 0))))
    (if entire
        (catalogue-mapitems
         (lambda ()
           (catalogue-give-up new-owner))
         (catalogue-list-item-set))
      (if (not (catalogue-native-p))
          (when (interactive-p)
            (error "This is not native item"))
        (dbf-set-this-record-modified-p t)
        (dbf-displayed-record-set-field 'since nil)
        (dbf-displayed-record-set-field-and-redisplay 'owner new-owner)
        (when (interactive-p)
          (db-save-database)
          (db-next-record 0)))))
  (if (interactive-p)
      (when (featurep 'emacspeak)
        (emacspeak-auditory-icon 'save-object))
    dbf-this-record-modified-p))

(defun catalogue-unregister (&optional entire)
  "Forget this item forever. When called with prefix argument
in data display buffer the action is applied to the entire item set.
In summary buffer prefix argument is not respected and action
is applied to the marked items if any or to the current one."
  (interactive "P")
  (let ((items nil)
        (processed 0))
    (cond
     ((db-summary-buffer-p)
      (dbs-in-data-display-buffer
       (setq items (catalogue-find-marked-records)
             processed (length items))
       (if items
           (when (or (not (interactive-p))
                     (y-or-n-p
                      (format "Forget %d marked item%s forever? "
                              processed (if (> processed 1) "s" ""))))
             (catalogue-delete items))
         (when (or (not (interactive-p))
                   (y-or-n-p "Forget this item forever? "))
           (catalogue-delete)
           (setq processed 1)))))
     (entire
      (when (or (not (interactive-p))
                (y-or-n-p "Forget this entire item set forever? "))
        (setq items (catalogue-list-item-set)
              processed (length items))
        (catalogue-delete items)))
     (t
      (when (or (not (interactive-p))
                (y-or-n-p "Forget this item forever? "))
        (catalogue-delete)
        (setq processed 1))))
    (unless (zerop processed)
      (db-save-database)
      (if (catalogue-empty-p)
          (dbf-kill-summary)
        (dbf-fill-summary-buffer-and-move-to-proper-record))
      (when (interactive-p)
        (when (featurep 'emacspeak)
          (emacspeak-auditory-icon 'delete-object))
        (message "%d deletion%s done" processed (if (> processed 1) "s" ""))))))


;;; That's all.

(provide 'catalogue-commands)
