;;; catalogue-view.el --- CD/DVD catalogue viewer

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
  (require 'database)
  (require 'db-summary))

(require 'easymenu)
(require 'database)
(require 'catalogue)


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

(defvar catalogue-affected-set nil
  "Stores association list of items and new set volumes.")

(defvar catalogue-operational-buffer-name nil
  "Catalogue operational buffer name.")

(defvar catalogue-current-item-set nil
  "Stores current item set volume after searching a place for new unit.")


;; Basic utility functions:

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

(defun catalogue-string-empty-p (str)
  "Return `t' if specified string is null or empty."
  (or (null str)
      (string= "" str)))

(defun catalogue-record-field-empty-p (field &optional record)
  "Check if specified field in displayed or specified record is empty."
  (catalogue-string-empty-p
   (record-field (or record (dbf-displayed-record)) field dbc-database)))

(defun catalogue-empty-p ()
  "Check if media catalogue database is empty."
  (and (= 1 (database-no-of-records dbc-database))
       (catalogue-record-field-empty-p 'id)))

(defun catalogue-native-p (&optional item)
  "Check if displayed or specified item is native."
  (catalogue-record-field-empty-p 'owner item))

(defun catalogue-released-p (&optional item)
  "Check if displayed or specified item is neither lended nor borrowed."
  (catalogue-record-field-empty-p 'since item))

(defun catalogue-lended-p (&optional item)
  "Check if displayed or specified item is lended."
  (not (catalogue-record-field-empty-p 'borrower item)))

(defun catalogue-borrowed-p (&optional item)
  "Check if displayed or specified item is borrowed."
  (not (or (catalogue-native-p item) (catalogue-released-p item))))

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

(defun catalogue-summary-synch-position ()
  "Synchronize position in summary buffer if any."
  (when (db-data-display-buffer-p)
    (dbf-in-summary-buffer
      (dbs-move-to-proper-record))))

(defun catalogue-find-hole-in-item-set (&optional item)
  "Search for a gap in the item set if any for specified or displayed record
and return first free unit number in the item set or nil if the set is full."
  (setq catalogue-current-item-set 0)
  (let ((units nil)
        (name (record-field (or item (dbf-displayed-record)) 'name dbc-database))
        (category (record-field (or item (dbf-displayed-record)) 'category dbc-database)))
    (maprecords
     (lambda (record)
       (when (and (string= name (record-field record 'name dbc-database))
                  (string= category (record-field record 'category dbc-database))
                  (or item (not (= maplinks-index dbc-index))))
         (push (record-field record 'unit dbc-database) units)
         (setq catalogue-current-item-set
               (max (record-field record 'set dbc-database)
                    catalogue-current-item-set))))
     dbc-database)
    (do ((x (sort units '<) (cdr x))
         (i 1 (1+ i)))
        ((or (null x) (< i (car x)))
         (if (> i catalogue-current-item-set)
             nil
           i)))))

(defun catalogue-count-set-amount (&optional item)
  "Return minimal correct value for the set amount
for specified or current item. If item is `nil' or not specified
then the currently displayed one is used to identify the set,
but it's unit number is not taken in account."
  (let ((amount
         (if item
             (record-field item 'unit dbc-database)
           0))
        (name (record-field (or item (dbf-displayed-record)) 'name dbc-database))
        (category (record-field (or item (dbf-displayed-record)) 'category dbc-database)))
    (maprecords
     (lambda (record)
       (and (string= name (record-field record 'name dbc-database))
            (string= category (record-field record 'category dbc-database))
            (not (= maplinks-index dbc-index))
            (setq amount (max (record-field record 'unit dbc-database) amount))))
     dbc-database)
    amount))

(defun catalogue-new-unit ()
  "Add a new unit to the item set of current record
correcting the `set' field. Return a number of added unit."
  (dbf-displayed-record-set-field
   'set
   (setq catalogue-current-item-set (1+ (catalogue-count-set-amount))))
  catalogue-current-item-set)

(defun catalogue-item-unique-p (&optional item)
  "Check whether the current item is unique by name, category and unit number.
If item is specified explicitly it is checked as it was the current one."
  (declare (special first-link))
  (let ((name (record-field (or item (dbf-displayed-record)) 'name dbc-database))
        (category (record-field (or item (dbf-displayed-record)) 'category dbc-database))
        (unit (record-field (or item (dbf-displayed-record)) 'unit dbc-database))
        (flag t))
    (maprecords
     (lambda (record)
       (when (and (string= name (record-field record 'name dbc-database))
                  (string= category (record-field record 'category dbc-database))
                  (= unit (record-field record 'unit dbc-database))
                  (not (= maplinks-index dbc-index)))
         (setq flag nil)
         (maprecords-break)))
     dbc-database)
    flag))

(defun catalogue-check-entry ()
  "Check currently editing  entry correctness
and issue corresponding error if needed."
  (let ((item (copy-record (dbf-displayed-record)))
        (field  (dbf-this-field-name))
        (suggest ", fix it first or discard changes"))
    (cond
     ((or (eq field 'name) (eq field 'category))
      (record-set-field
       item field
       (buffer-substring (dbf-this-field-beginning-pos) (dbf-this-field-end-pos))
       dbc-database))
     ((or (eq field 'unit) (eq field 'set))
      (record-set-field
       item field
       (string-to-number
        (buffer-substring (dbf-this-field-beginning-pos) (dbf-this-field-end-pos)))
       dbc-database))
     (t nil))
    (when (catalogue-record-field-empty-p 'name item)
      (error "Empty name is not allowed%s" suggest))
    (when (catalogue-record-field-empty-p 'category item)
      (error "Empty category is not allowed%s" suggest))
    (unless (catalogue-item-unique-p item)
      (error "Duplicate entry%s" suggest))
    (when (< (record-field item 'unit dbc-database) 1)
      (error "Unit number must be > 0%s" suggest))
    (let ((amount (catalogue-count-set-amount item)))
      (when (< (record-field item 'set dbc-database) amount)
        (error "This set consists at least of %d units%s" amount suggest)))))


;; EDB hooks:

(defun catalogue-setup ()
  "Setup media catalogue database."
  (declare (special catalogue-view-map))
  (setq catalogue-unknown-disk nil)
  (unless (file-exists-p catalogue-db-file)
    (db-toggle-internal-file-layout t))
  (use-local-map catalogue-view-map))

(defun catalogue-view-setup ()
  "Setup view mode."
  (declare (special catalogue-view-map catalogue-preview-map))
  (if (not catalogue-unknown-disk)
      (use-local-map catalogue-view-map)
    (require 'catalogue-media)
    (use-local-map catalogue-preview-map)))

(defun catalogue-edit-setup ()
  "Setup record editing mode."
  (declare (special catalogue-edit-map))
  (require 'catalogue-edit)
  (use-local-map catalogue-edit-map))

(defun catalogue-summary-setup ()
  "Summary mode setup."
  (declare (special catalogue-summary-map))
  (require 'catalogue-summary)
  (use-local-map catalogue-summary-map))

(defun catalogue-choose-display-format (record)
  "Choose an appropriate display format for the record."
  (let ((db-format-file-path catalogue-display-format-path))
    (cond
     ((string= catalogue-operational-buffer-name (buffer-name))
      (db-change-format "operational"))
     (catalogue-editing-p
      (let ((affected (assoc dbc-index catalogue-affected-set)))
        (when affected
          (dbf-displayed-record-set-field 'set (cdr affected))))
      (db-change-format "edit"))
     (catalogue-unknown-disk
      (db-change-format "registration"))
     ((catalogue-empty-p)
      (db-change-format "empty"))
     ((catalogue-lended-p record)
      (db-change-format
       (if (catalogue-native-p record)
           "lended"
         "transit")))
     ((not (catalogue-native-p record))
      (db-change-format
       (if (catalogue-released-p record)
           "alien"
         "borrowed")))
     (t (db-change-format "native")))))

(defun catalogue-initialize-record (record database)
  "Initialize newly created record."
  (record-set-field record 'id catalogue-no-id database))

(defun catalogue-validate-field-change (field old new)
  "Validate mandatory fields change. Intended for field change hook."
  (cond
   ((eq field 'name)
    (if (catalogue-string-empty-p new)
        (progn
          (dbf-displayed-record-set-field 'name old)
          (ding)
          (message "Empty name is not allowed")
          t)
      (dbf-displayed-record-set-field
       'unit (or (catalogue-find-hole-in-item-set)
                 (catalogue-new-unit)))
      (dbf-displayed-record-set-field 'set catalogue-current-item-set)
      t))
   ((eq field 'category)
    (if (catalogue-string-empty-p new)
        (progn
          (dbf-displayed-record-set-field 'category old)
          (ding)
          (message "Empty category is not allowed")
          t)
      (dbf-displayed-record-set-field
       'unit (or (catalogue-find-hole-in-item-set)
                 (catalogue-new-unit)))
      (dbf-displayed-record-set-field 'set catalogue-current-item-set)
      t))
   ((eq field 'unit)
    (if (> new 0)
        (if (catalogue-item-unique-p)
            (if (<= new (dbf-displayed-record-field 'set))
                nil
              (dbf-displayed-record-set-field 'set new)
              t)
          (dbf-displayed-record-set-field 'unit old)
          (ding)
          (message "Duplicate item")
          t)
      (dbf-displayed-record-set-field 'unit old)
      (ding)
      (message "Unit number must be > 0")
      t))
   ((eq field 'set)
    (let ((amount (catalogue-count-set-amount (dbf-displayed-record))))
      (if (>= new amount)
          nil
        (dbf-displayed-record-set-field 'set amount)
        (ding)
        (message "This set can not be further shrinked")
        t)))
   (t nil)))

(defun catalogue-accept-record (record)
  "Some catalogue specific actions concerning record commitment."
  (unless (string= catalogue-operational-buffer-name (buffer-name))
    (setq catalogue-unknown-disk nil)
    (let ((name (record-field record 'name dbc-database))
          (category (record-field record 'category dbc-database))
          (amount (record-field record 'set dbc-database)))
      (maprecords
       (lambda (record)
         (when (and (string= name (record-field record 'name dbc-database))
                    (string= category (record-field record 'category dbc-database))
                    (not (= maplinks-index dbc-index)))
           (let ((listed (assoc maplinks-index catalogue-affected-set)))
             (if listed
                 (setcdr listed amount)
               (push (cons maplinks-index amount) catalogue-affected-set)))))
       dbc-database))))


;;; Interactive commands:

;; Main entry point:

(defun catalogue-view ()
  "Enter the collection catalogue."
  (interactive)
  (setq catalogue-unknown-disk nil)
  (setq catalogue-editing-p nil)
  (catalogue-db-open)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)))


;; Browsing functions:

(put 'end-of-catalogue 'error-conditions '(error end-of-catalogue))
(put 'end-of-catalogue 'error-message "End of catalogue")

(defun catalogue-next-record (&optional arg)
  "Go to the next catalogue record wrapping around the database if enabled.
With prefix argument jumps to the next item set."
  (interactive "P")
  (declare (special first-link))
  (when catalogue-editing-p
    (db-in-data-display-buffer
      (when (eq dbf-minor-mode 'edit)
        (catalogue-check-entry))))
  (if (and (not catalogue-database-wraparound)
           (= (catalogue-index) (database-no-of-records dbc-database)))
      (signal 'end-of-catalogue nil)
    (if arg
        (let ((name (catalogue-this-record-field 'name))
              (found nil))
          (maprecords
           (lambda (record)
             (and (> maplinks-index (catalogue-index))
                  (not (string= (record-field record 'name dbc-database) name))
                  (setq found maplinks-index)
                  (maprecords-break)))
           dbc-database)
          (if found
              (db-jump-to-record found)
            (if catalogue-database-wraparound
                (db-first-record)
              (signal 'end-of-catalogue nil))))
      (db-next-record 1))
    (catalogue-summary-synch-position)
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon 'scroll)
      (emacspeak-speak-current-window))))

(defun catalogue-next-category ()
  "Jump to the next category wrapping around the database if enabled."
  (interactive)
  (declare (special first-link))
  (when catalogue-editing-p
    (db-in-data-display-buffer
      (when (eq dbf-minor-mode 'edit)
        (catalogue-check-entry))))
  (let ((category (catalogue-this-record-field 'category))
        (found nil))
    (maprecords
     (lambda (record)
       (and (> maplinks-index (catalogue-index))
            (not (string= (record-field record 'category dbc-database) category))
            (setq found maplinks-index)
            (maprecords-break)))
     dbc-database)
    (if found
        (db-jump-to-record found)
      (if catalogue-database-wraparound
          (db-first-record)
        (signal 'end-of-catalogue nil))))
  (catalogue-summary-synch-position)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (dbf-displayed-record-field 'category))))

(put 'beginning-of-catalogue 'error-conditions '(error beginning-of-catalogue))
(put 'beginning-of-catalogue 'error-message "Beginning of catalogue")

(defun catalogue-previous-record (&optional arg)
  "Go to the previous catalogue record wrapping around the database if enabled.
With prefix argument jumps to the previous item set."
  (interactive "P")
  (declare (special first-link))
  (when catalogue-editing-p
    (db-in-data-display-buffer
      (when (eq dbf-minor-mode 'edit)
        (catalogue-check-entry))))
  (if (and (not catalogue-database-wraparound)
           (= (catalogue-index) 1))
      (signal 'beginning-of-catalogue nil)
    (if arg
        (let* ((name (catalogue-this-record-field 'name))
               (prev name)
               (new name)
               (found nil))
          (maprecords
           (lambda (record)
             (setq new (record-field record 'name dbc-database))
             (if (and (not catalogue-database-wraparound)
                      (or (>= maplinks-index (catalogue-index))
                          (string= new name)))
                 (maprecords-break)
               (unless (string= prev new)
                 (setq found maplinks-index
                       prev new))))
           dbc-database)
          (if found
              (db-jump-to-record found)
            (if catalogue-database-wraparound
                (db-first-record)
              (signal 'beginning-of-catalogue nil))))
      (db-previous-record 1))
    (catalogue-summary-synch-position)
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon 'scroll)
      (emacspeak-speak-current-window))))

(defun catalogue-previous-category ()
  "Jump to the previous category wrapping around the database if enabled."
  (interactive)
  (declare (special first-link))
  (when catalogue-editing-p
    (db-in-data-display-buffer
      (when (eq dbf-minor-mode 'edit)
        (catalogue-check-entry))))
  (let* ((category (catalogue-this-record-field 'category))
         (prev category)
         (new category)
         (found nil))
    (maprecords
     (lambda (record)
       (setq new (record-field record 'category dbc-database))
       (if (and (not catalogue-database-wraparound)
                (or (>= maplinks-index (catalogue-index))
                    (string= new category)))
           (maprecords-break)
         (unless (string= prev new)
           (setq found maplinks-index
                 prev new))))
     dbc-database)
    (if found
        (db-jump-to-record found)
      (if catalogue-database-wraparound
          (db-first-record)
        (signal 'beginning-of-catalogue nil))))
  (catalogue-summary-synch-position)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (dbf-displayed-record-field 'category))))

(defun catalogue-exit ()
  "Exit catalogue and kill all it's buffers."
  (interactive)
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (db-exit t)))
   (database-data-display-buffers dbc-database))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))


;; Autoloaded functions from other modules:

(autoload 'catalogue-edit "catalogue-edit" "Edit current catalogue record." t)
(autoload 'catalogue-add-item "catalogue-edit" "Add a new catalogue item manually." t)

(autoload 'catalogue-reassign "catalogue-media" "Reassign current catalogue record to the inserted disk." t)
(autoload 'catalogue-open-tray "catalogue-media" "Open the disk tray." t)

(autoload 'catalogue-borrow "catalogue-commands" "Register item in catalogue as borrowed." t)
(autoload 'catalogue-lend "catalogue-commands" "Register item in catalogue as lended." t)
(autoload 'catalogue-release "catalogue-commands" "Release borrowed or lended item." t)
(autoload 'catalogue-give-up "catalogue-commands" "Register item in catalogue as alien." t)
(autoload 'catalogue-unregister "catalogue-commands" "Forget this item forever." t)

(autoload 'catalogue-summary "catalogue-summary" "Pop up summary window or synchronize it.")

(autoload 'catalogue-search "catalogue-search" "Search record by specified field and pattern." t)


;; Key bindings for catalogue viewing:

(defvar catalogue-view-map (make-keymap)
  "Catalogue view mode keymap.")
(suppress-keymap catalogue-view-map t)
(loop for binding in 
      '(("/" . catalogue-disk-identify)
        ("I" . catalogue-disk-identify)
        ([return] . catalogue-edit)
        ("e" . catalogue-edit)
        ("a" . catalogue-add-item)
        ("s" . catalogue-search)
        ("B" . catalogue-borrow)
        ("L" . catalogue-lend)
        ("R" . catalogue-release)
        ("G" . catalogue-give-up)
        ("A" . catalogue-acquire)
        ("o" . catalogue-open-tray)
        ("\C-cr" . catalogue-reassign)
        ("\C-d" . catalogue-unregister)
        ("?" . describe-mode)
        ([next] . catalogue-next-record)
        ("n" . catalogue-next-record)
        ([C-next] . catalogue-next-category)
        ("\C-n" . catalogue-next-category)
        ([prior] . catalogue-previous-record)
        ("p" . catalogue-previous-record)
        ([C-prior] . catalogue-previous-category)
        ("\C-p" . catalogue-previous-category)
        ([C-home] . db-first-record)
        ([C-end] . db-last-record)
        ("S" . catalogue-summary)
        ("q" . catalogue-exit))
      do
      (define-key catalogue-view-map (car binding) (cdr binding)))


;; Catalogue view menu:

(easy-menu-define nil catalogue-view-map
  "Media catalogue view menu"
  '("Catalogue"
    ["Edit" catalogue-edit (not (catalogue-empty-p))]
    ["Add item" catalogue-add-item t]
    ["Delete" catalogue-unregister (not (catalogue-empty-p))]
    "-----"
    ["Search" catalogue-search (not (catalogue-empty-p))]
    "-----"
    ["Identify disk" catalogue-disk-identify t]
    ["Reassign" catalogue-reassign (not (catalogue-empty-p))]
    ["Open tray" catalogue-open-tray t]
    "-----"
    ["Borrow" catalogue-borrow (not (catalogue-empty-p))]
    ["Lend" catalogue-lend (not (catalogue-empty-p))]
    ["Release" catalogue-release (not (catalogue-empty-p))]
    ["Give up" catalogue-give-up (not (catalogue-empty-p))]
    ["Acquire" catalogue-acquire (not (catalogue-empty-p))]
    "-----"
    ["Summary" catalogue-summary (not (catalogue-empty-p))]
    ["Quit" catalogue-exit t]))


;;; That's all.

(provide 'catalogue-view)
