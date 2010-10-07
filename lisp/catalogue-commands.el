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


;;; Interactive commands:

(defun catalogue-borrow (&optional entire)
  "Register this disk as borrowed.
With prefix argument apply the action to the entire disk set."
  (interactive "P")
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when (catalogue-native-p)
    (error "This disk is native, so cannot be borrowed"))
  (when (catalogue-borrowed-p)
    (error "This disk is already borrowed"))
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field-and-redisplay 'since
                                                (format-time-string
                                                 catalogue-date-format))
  (when entire
    (let ((name (dbf-displayed-record-field 'name)))
      (maprecords
       (lambda (record)
         (and (string= name (record-field record 'name dbc-database))
              (not (string= "" (record-field record 'owner dbc-database)))
              (string= "" (record-field record 'since dbc-database))
              (record-set-field record 'since
                                (format-time-string catalogue-date-format)
                                dbc-database)))
       dbc-database)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun catalogue-lend (lender &optional entire)
  "Register this disk as lended.
With prefix argument apply the action to the entire disk set."
  (interactive "sLender: \nP")
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field 'since
                                  (format-time-string catalogue-date-format))
  (dbf-displayed-record-set-field-and-redisplay 'lended lender)
  (when entire
    (let ((name (dbf-displayed-record-field 'name)))
      (maprecords
       (lambda (record)
         (when (string= name (record-field record 'name dbc-database))
           (record-set-field record 'since
                             (format-time-string catalogue-date-format)
                             dbc-database)
           (record-set-field record 'lended lender dbc-database)))
       dbc-database)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun catalogue-release (&optional entire)
  "Release borrowed or lended item.
With prefix argument do this action on the entire set."
  (interactive "P")
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field 'lended nil)
  (dbf-displayed-record-set-field-and-redisplay 'since nil)
  (when entire
    (let ((name (dbf-displayed-record-field 'name)))
      (maprecords
       (lambda (record)
         (when (string= name (record-field record 'name dbc-database))
           (record-set-field record 'lended nil dbc-database)
           (record-set-field record 'since nil dbc-database)))
       dbc-database)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun catalogue-give-up (new-owner &optional entire)
  "Register this disk as alien.
With prefix argument apply the action to the entire disk set."
  (interactive "sNew owner: \nP")
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field 'since nil)
  (dbf-displayed-record-set-field-and-redisplay 'owner new-owner)
  (when entire
    (let ((name (dbf-displayed-record-field 'name)))
      (maprecords
       (lambda (record)
         (when (string= name (record-field record 'name dbc-database))
           (record-set-field record 'since nil dbc-database)
           (record-set-field record 'owner new-owner dbc-database)))
       dbc-database)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun catalogue-unregister (&optional entire)
  "Forget this disk forever.
With prefix argument unregisters entire disk set."
  (interactive "P")
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when (y-or-n-p
         (format "Forget this %s forever? "
                 (if entire
                     "entire disk set"
                   "disk")))
    (if entire
        (let ((name (dbf-displayed-record-field 'name))
              (index 0))
          (maprecords
           (lambda (record)
             (setq index (1+ index))
             (when (string= name
                            (record-field record 'name dbc-database))
               (maprecords-break)))
           dbc-database)
          (db-jump-to-record index)
          (while (string= (dbf-displayed-record-field 'name) name)
            (catalogue-delete-record)))
      (catalogue-delete-record))
    (db-save-database)
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon 'delete-object))))


;;; That's all.

(provide 'catalogue-commands)
