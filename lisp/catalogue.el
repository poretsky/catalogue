;;; catalogue.el --- CD/DVD collection holder

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

(eval-when-compile (require 'cl))
(require 'custom)
(require 'database)
(require 'catalogue-keymap)


;;; Customizations:

(defgroup catalogue nil
  "Catalogue: CD/DVD catalogue database"
  :prefix "catalogue-"
  :group 'applications)

(defcustom catalogue-database-language nil
  "Language used for database display."
  :type '(radio (const :tag "Examine environment" nil)
                (const :tag "English" "en")
                (const :tag "Russian" "ru"))
  :group 'catalogue)

(defcustom catalogue-database-wraparound nil
  "Allow wrapping around the database when browsing it by records."
  :type 'boolean
  :group 'catalogue)

(defcustom catalogue-record-wraparound nil
  "Allow wrapping around a record when moving from field to field
in editing mode."
  :type 'boolean
  :group 'catalogue)

(defcustom catalogue-cd-dvd-device (expand-file-name "/dev/cdrom")
  "*CD/DVD device."
  :type 'file
  :group 'catalogue)

(defcustom catalogue-cd-dvd-mountpoint (expand-file-name "/cdrom/")
  "*CD/DVD mountpoint.
It should be listed in fstab
and accessible to the database user."
  :type 'directory
  :group 'catalogue)

(defcustom catalogue-meaningful-files-quota 0.7
  "*Quota of minimum disk space occupied by category sensible files."
  :type 'number
  :group 'catalogue)

(defcustom catalogue-media-coding-system nil
  "The coding system used when mounting CD or DVD.
It should correspond to the `iocharset' mount option.
Usually there is no need to set this option here."
  :type '(coding-system :size 0)
  :group 'catalogue)


;;; Code:

(defconst catalogue-shared-resource-path
  (list "/usr/local/share/catalogue"
        "/usr/share/catalogue"
        (expand-file-name "../db" (file-name-directory load-file-name)))
  "List of common resource paths.")

(defconst catalogue-resource-directory (expand-file-name "~/.catalogue/")
  "User database directory.")

(defconst catalogue-db-file (expand-file-name "collection.dat" catalogue-resource-directory)
  "User database file.")

(defconst catalogue-no-id "noid"
  "Dummy disk identifier.")


(defvar catalogue-display-format-path nil
  "Path list to the format files for various display modes.")

(defvar catalogue-unknown-disk nil
  "Indicates whether this disk has to be registered.")

(defvar catalogue-editing-p nil
  "Edit mode indicator.")

(defvar catalogue-searching-p nil
  "This variable is used internally by the search functions
to force choosing special displayspec the list of searchable fields.
It should be `nil' outside this.")


(defun catalogue-db-open ()
  "Open existing user database or create a fresh one."
  (unless (file-exists-p catalogue-resource-directory)
    (make-directory catalogue-resource-directory))
  (let ((db-format-file-path catalogue-shared-resource-path)
        (db-aux-file-path catalogue-shared-resource-path))
    (db-find-file catalogue-db-file)))

(defun catalogue-language ()
  "get current database language as a valid two-letter code."
  (or catalogue-database-language
      (let ((lang (or (getenv "LANG") "")))
        (cond
         ((string-match "^ru_RU" lang) "ru")
         (t "en")))))

(defun catalogue-empty-p ()
  "Check if media catalogue database is empty."
  (and (= 1 (database-no-of-records dbc-database))
       (or (null (dbf-displayed-record-field 'id))
           (string= (dbf-displayed-record-field 'id) ""))))

(defun catalogue-index ()
  "Retrieve current record index in the database."
  (if (db-summary-buffer-p)
      dbs-index
    dbc-index))

(defun catalogue-this-record-field (fieldname)
  "Get field value by name from the current record.
Works in summary buffer as well."
  (if (db-summary-buffer-p)
      (dbs-in-data-display-buffer
       (dbf-displayed-record-field fieldname))
    (dbf-displayed-record-field fieldname)))

(defun catalogue-setup ()
  "Setup media catalogue database."
  (setq catalogue-unknown-disk nil)
  (unless (file-exists-p catalogue-db-file)
    (db-toggle-internal-file-layout t))
  (use-local-map catalogue-view-map))

(defun catalogue-view-setup ()
  "Setup view mode."
  (if catalogue-unknown-disk
      (use-local-map catalogue-preview-map)
    (use-local-map catalogue-view-map)))

(defun catalogue-edit-setup ()
  "Setup record editing mode."
  (use-local-map catalogue-edit-map))

(defun catalogue-summary-setup ()
  "Summary mode setup."
  (use-local-map catalogue-summary-map))

(defun catalogue-choose-display-format (record)
  "Choose an appropriate display format for the record."
  (let ((db-format-file-path catalogue-display-format-path))
    (cond
     (catalogue-searching-p
      (db-change-format "searchable fields"))
     (catalogue-editing-p
      (db-change-format "edit disk info"))
     (catalogue-unknown-disk
      (db-change-format "disk registration form"))
     ((catalogue-empty-p)
      (db-change-format "empty"))
     ((and (record-field record 'lended dbc-database)
           (not (string= (record-field record 'lended dbc-database) "")))
      (if (or (not (record-field record 'owner dbc-database))
              (string= (record-field record 'owner dbc-database) ""))
          (db-change-format "lended disk info")
        (db-change-format "transit disk info")))
     ((and (record-field record 'owner dbc-database)
           (not (string= (record-field record 'owner dbc-database) "")))
      (if (or (not (record-field record 'since dbc-database))
              (string= (record-field record 'since dbc-database) ""))
          (db-change-format "alien disk info")
        (db-change-format "borrowed disk info")))
     (t (db-change-format "native disk info")))))

(defun catalogue-initialize-record (record database)
  "Initialize newly created record."
  (record-set-field record 'id catalogue-no-id database))

(defun catalogue-delete-record ()
  "Delete record or clear it if it is the only one."
  (when (= 1 (database-no-of-records dbc-database))
    (db-add-record)
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field 'id "")
    (db-view-mode)
    (db-next-record 1))
  (db-delete-record t))

(defun catalogue-find-hole-in-disk-set (name)
  "Return first free unit number in the disk set or nil if the set is full."
  (let ((units nil)
        (limit 0))
    (maprecords
     (lambda (record)
       (when (string= name
                      (record-field record 'name dbc-database))
         (setq units
               (append units
                       (list (record-field record 'unit dbc-database))))
         (setq limit (record-field record 'set dbc-database))))
     dbc-database)
    (do ((x (sort units '<) (cdr x))
         (i 1 (1+ i)))
        ((or (null x) (< i (car x)))
         (if (> i limit)
             nil
           i)))))

(defun catalogue-validate-field-change (field old new)
  "Field change validator.
Ensures that the new unit number is within the disk set.
Intended for use in the field change hook."
  (cond
   ((eq field 'unit)
    (if (and (>= new 1)
             (<= new (dbf-displayed-record-field 'set)))
        nil
      (dbf-displayed-record-set-field field old)
      (message "Unit number is out of range")
      (ding)
      t))
   ((eq field 'set)
    (let ((maxunit (dbf-displayed-record-field 'unit))
          (name (dbf-displayed-record-field 'name)))
      (maprecords
       (lambda (record)
         (and (string= name (record-field record 'name dbc-database))
              (< maxunit
                 (record-field record 'unit dbc-database))
              (setq maxunit
                    (record-field record 'unit dbc-database))))
       dbc-database)
      (if (>= new maxunit)
          (maprecords
           (lambda (record)
             (and (string= name
                           (record-field record 'name dbc-database))
                  (record-set-field record 'set new dbc-database)))
           dbc-database)
        (dbf-displayed-record-set-field field old)
        (message "Cannot shrink this disk set")
        (ding)
        t)))
   ((eq field 'name)
    (if (and new (not (string= "" new)))
        (let ((name nil)
              (set (dbf-displayed-record-field 'set))
              (unit (dbf-displayed-record-field 'unit)))
          (maprecords
           (lambda (record)
             (when (string= new (record-field record 'name dbc-database))
               (setq name new)
               (setq set (record-field record 'set dbc-database))
               (maprecords-break)))
           dbc-database)
          (if (null name)
              nil
            (if (and (> set 1)
                     (setq unit (catalogue-find-hole-in-disk-set name)))
                (progn
                  (dbf-displayed-record-set-field 'set set)
                  (dbf-displayed-record-set-field 'unit unit)
                  t)
              (dbf-displayed-record-set-field field old)
              (message "The name is not unique")
              (ding)
              nil)))
      (dbf-displayed-record-set-field field old)
      (message "Empty name")
      (ding)
      nil))
   (t nil)))

(defun catalogue-accept-record (record)
  "Some catalogue specific actions concerning record commitment."
  (setq catalogue-unknown-disk nil))

(defun catalogue-native-p ()
  "Check if displayed disk is native."
  (string= "" (dbf-displayed-record-field 'owner)))

(defun catalogue-borrowed-p ()
  "Check if displayed disk is borrowed."
  (and (dbf-displayed-record-field 'owner)
       (not (string= "" (dbf-displayed-record-field 'owner)))
       (dbf-displayed-record-field 'since)
       (not (string= "" (dbf-displayed-record-field 'since)))))


;;; Interactive commands:

;; Main entry point:

(defun catalogue-view ()
  "Enter the disks catalogue."
  (interactive)
  (setq catalogue-unknown-disk nil)
  (setq catalogue-editing-p nil)
  (setq catalogue-searching-p nil)
  (catalogue-db-open)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)))


;; Browsing functions:

(put 'end-of-catalogue 'error-conditions '(error end-of-catalogue))
(put 'end-of-catalogue 'error-message "End of catalogue")

(defun catalogue-next-record (&optional arg)
  "Go to the next catalogue record wrapping around the database if enabled.
With prefix argument jumps to the next disk set."
  (interactive "P")
  (if (and (not catalogue-database-wraparound)
           (= (catalogue-index) (database-no-of-records dbc-database)))
      (signal 'end-of-catalogue nil)
    (if arg
        (let ((name (catalogue-this-record-field 'name))
              (index 0)
              (found nil))
          (maprecords
           (lambda (record)
             (and (> (setq index (1+ index)) (catalogue-index))
                  (setq found (not (string= (record-field record 'name dbc-database) name)))
                  (maprecords-break)))
           dbc-database)
          (if found
              (db-jump-to-record index)
            (if catalogue-database-wraparound
                (db-first-record)
              (signal 'end-of-catalogue nil))))
      (db-next-record 1))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon
       (if (eq dbf-minor-mode 'edit)
           'scroll
         'select-object))
      (emacspeak-speak-line))))

(defun catalogue-next-category ()
  "Jump to the next disk category wrapping around the database if enabled."
  (interactive)
  (let ((category (catalogue-this-record-field 'category))
        (index 0)
        (found nil))
    (maprecords
     (lambda (record)
       (and (> (setq index (1+ index)) (catalogue-index))
            (setq found (not (string= (record-field record 'category dbc-database) category)))
            (maprecords-break)))
     dbc-database)
    (if found
        (db-jump-to-record index)
      (if catalogue-database-wraparound
          (db-first-record)
        (signal 'end-of-catalogue nil))))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(put 'beginning-of-catalogue 'error-conditions '(error beginning-of-catalogue))
(put 'beginning-of-catalogue 'error-message "Beginning of catalogue")

(defun catalogue-previous-record (&optional arg)
  "Go to the previous catalogue record wrapping around the database if enabled.
With prefix argument jumps to the previous disk set."
  (interactive "P")
  (if (and (not catalogue-database-wraparound)
           (= (catalogue-index) 1))
      (signal 'beginning-of-catalogue nil)
    (if arg
        (let* ((name (catalogue-this-record-field 'name))
               (prev name)
               (new name)
               (index 0)
               (found nil))
          (maprecords
           (lambda (record)
             (setq new (record-field record 'name dbc-database)
                   index (1+ index))
             (if (and (not catalogue-database-wraparound)
                      (or (>= index (catalogue-index))
                          (string= new name)))
                 (maprecords-break)
               (unless (string= prev new)
                 (setq found index
                       prev new))))
           dbc-database)
          (if found
              (db-jump-to-record found)
            (if catalogue-database-wraparound
                (db-first-record)
              (signal 'beginning-of-catalogue nil))))
      (db-previous-record 1))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon
       (if (eq dbf-minor-mode 'edit)
           'scroll
         'select-object))
      (emacspeak-speak-line))))

(defun catalogue-previous-category ()
  "Jump to the previous disk category wrapping around the database if enabled."
  (interactive)
  (let* ((category (catalogue-this-record-field 'category))
         (prev category)
         (new category)
         (index 0)
         (found nil))
    (maprecords
     (lambda (record)
       (setq new (record-field record 'category dbc-database)
             index (1+ index))
       (if (and (not catalogue-database-wraparound)
                (or (>= index (catalogue-index))
                    (string= new category)))
           (maprecords-break)
         (unless (string= prev new)
           (setq found index
                 prev new))))
     dbc-database)
    (if found
        (db-jump-to-record found)
      (if catalogue-database-wraparound
          (db-first-record)
        (signal 'beginning-of-catalogue nil))))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))


;; Editing functions:

(defun catalogue-edit ()
  "Set display format convenient for editing."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when (and (not catalogue-unknown-disk)
             (catalogue-empty-p))
    (error "Catalogue is empty"))
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
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (unless (eq dbf-minor-mode 'edit)
    (error "Not in editing mode"))
  (setq catalogue-editing-p nil)
  (db-sort t)
  (db-save-database)
  (db-view-mode)
  (db-next-record 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun catalogue-cancel ()
  "Finish editing without saving changes."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (unless (eq dbf-minor-mode 'edit)
    (error "Not in editing mode"))
  (dbf-set-this-field-modified-p nil)
  (dbf-set-this-record-modified-p nil)
  (dbc-set-database-modified-p nil)
  (let ((index (catalogue-index)))
    (db-exit t)
    (catalogue-view)
    (db-jump-to-record (min (database-no-of-records dbc-database) index)))
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


;;; That's all.

(provide 'catalogue)
