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

(eval-when-compile
  (require 'database)
  (require 'db-summary))

(require 'easymenu)
(require 'database)
(require 'catalogue-view)
(require 'catalogue-util)


;;; Code:

(defun catalogue-summary-mark-set (arg value)
  "Set mark value on current record or on full item set
depending on arg. If arg is not `nil' and not `set' it is treated
as name of field by which record should be selected.
At the end of operation position is advanced to the next
available record after the last processed one."
  (unless (db-summary-buffer-p)
    (error "Not in summary buffer"))
  (if (null arg)
      (db-mark-record value)
    (dbs-in-data-display-buffer
      (mapc
       (lambda (item)
         (db-select-record item)
         (db-mark-record value))
       (nreverse
        (if (eq arg 'set)
            (catalogue-list-item-set)
          (catalogue-list-the-same arg)))))
    (dbs-move-to-proper-record))
  (condition-case nil
      (catalogue-next-record)
    (end-of-catalogue nil)))

(defun catalogue-summary-mark-filter (predicate marked-only)
  "Filter records according to specified predicate.
If second argument is not `nil' and some records are marked
before calling this function then those of them unsatisfying
the predicate will be unmarked. Otherwise all records satisfying
predicate will be marked. If some hits are found pointer is moved
to the first one and `t' is returned. Otherwise pointer remains
on it's original position and `nil' is returned."
  (unless (db-summary-buffer-p)
    (error "Not in summary buffer"))
  (dbs-in-data-display-buffer
    (let ((items (catalogue-find-marked-records))
          (hits 0)
          (first-hit))
      (when (and marked-only items)
        (db-unmark-all))
      (maprecords
       (lambda (record)
         (when (and (funcall predicate record)
                    (or (not marked-only)
                        (null items)
                        (member maplinks-index items)))
           (db-select-record maplinks-index)
           (db-mark-record 1)
           (when (zerop hits)
             (setq first-hit maplinks-index))
           (setq hits (1+ hits))))
       dbc-database)
      (message "%s hit%s found"
               (if (zerop hits)
                   "No"
                 (db-jump-to-record first-hit)
                 (dbf-fill-summary-buffer-and-move-to-proper-record)
                 (format "%d" hits))
               (if (= 1 hits)
                   ""
                 "s"))
      (not (zerop hits)))))


;;; Interactive commands:

;; Entry point:

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


;; Marking:

(defun catalogue-summary-mark (&optional arg)
  "Mark current record or full item set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set (and arg 'set) 1)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-unmark (&optional arg)
  "Unmark current record or full item set if called with prefix argument.
Position is advanced to the next record."
  (interactive "P")
  (catalogue-summary-mark-set (and arg 'set) 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-mark-category ()
  "Mark all records of category the current one belongs to.
Position is advanced to the next record."
  (interactive)
  (catalogue-summary-mark-set 'category 1)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-unmark-category ()
  "Unmark all records of category the current one belongs to.
Position is advanced to the next record."
  (interactive)
  (catalogue-summary-mark-set 'category 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))


;; Filtering:

(defun catalogue-summary-filter-alien (&optional arg)
  "Filter alien items. With prefix argument apply selection
to the marked items if any unmarking non-alien ones."
  (interactive "P")
  (let ((found
         (catalogue-summary-mark-filter
          (lambda (record)
            (not (catalogue-native-p record)))
          arg)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon (if found 'search-hit 'search-miss))
      (when found
        (emacspeak-speak-line)))))

(defun catalogue-summary-filter-native (&optional arg)
  "Filter native items. With prefix argument apply selection
to the marked items if any unmarking non-native ones."
  (interactive "P")
  (let ((found (catalogue-summary-mark-filter 'catalogue-native-p arg)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon (if found 'search-hit 'search-miss))
      (when found
        (emacspeak-speak-line)))))

(defun catalogue-summary-filter-borrowed (&optional arg)
  "Filter borrowed items. With prefix argument apply selection
to the marked items if any unmarking not borrowed ones."
  (interactive "P")
  (let ((found (catalogue-summary-mark-filter 'catalogue-borrowed-p arg)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon (if found 'search-hit 'search-miss))
      (when found
        (emacspeak-speak-line)))))

(defun catalogue-summary-filter-lended (&optional arg)
  "Filter lended items. With prefix argument apply selection
to the marked items if any unmarking not lended ones."
  (interactive "P")
  (let ((found (catalogue-summary-mark-filter 'catalogue-lended-p arg)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon (if found 'search-hit 'search-miss))
      (when found
        (emacspeak-speak-line)))))


;; Navigation:

(put 'no-more-marks 'error-conditions '(error no-more-marks))
(put 'no-more-marks 'error-message "No more marks")

(defun catalogue-summary-next-record (&optional arg)
  "Go to the next record in summary buffer.
with prefix argument go to the next item set."
  (interactive "P")
  (catalogue-next-record arg)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-next-mark ()
  "Go to the next marked record."
  (interactive)
  (let ((marks (catalogue-find-marked-records)))
    (if (and marks
             (or catalogue-database-wraparound
                 (< (catalogue-index) (car marks))))
        (db-next-marked-record 1)
      (signal 'no-more-marks nil)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-previous-record (&optional arg)
  "Go to the previous record in summary buffer.
With prefix argument go to the previous item set."
  (interactive "P")
  (catalogue-previous-record arg)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-previous-mark ()
  "Go to the previous marked record."
  (interactive)
  (let ((marks (catalogue-find-marked-records)))
    (if (and marks
             (or catalogue-database-wraparound
                 (> (catalogue-index) (car (nreverse marks)))))
        (db-previous-marked-record 1)
      (signal 'no-more-marks nil)))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defun catalogue-summary-first-record ()
  "Go to the first record in summary buffer."
  (interactive)
  (dbs-in-data-display-buffer
    (db-first-record)
    (catalogue-summary-synch-position))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-last-record ()
  "Go to the last record in summary buffer."
  (interactive)
  (dbs-in-data-display-buffer
    (db-last-record)
    (catalogue-summary-synch-position))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-scroll-up ()
  "Scroll up in summary buffer."
  (interactive)
  (dbs-scroll-up)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defun catalogue-summary-scroll-down ()
  "Scroll down in summary buffer."
  (interactive)
  (dbs-scroll-down)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))


;; Autoloads:

(autoload 'catalogue-report "catalogue-report" "Make a short report of the collection catalogue." t)


;; Key bindings for summary view:

(defvar catalogue-summary-map (make-keymap)
  "catalogue summary mode keymap.")
(suppress-keymap catalogue-summary-map t)
(loop for binding in
      '(([down] . catalogue-summary-next-record)
        ("n" . catalogue-summary-next-record)
        ([up] . catalogue-summary-previous-record)
        ("p" . catalogue-summary-previous-record)
        ([C-down] . catalogue-summary-next-mark)
        ("\C-n" . catalogue-summary-next-mark)
        ([C-up] . catalogue-summary-previous-mark)
        ("\C-p" . catalogue-summary-previous-mark)
        ([next] . catalogue-summary-scroll-up)
        ([prior] . catalogue-summary-scroll-down)
        ([C-home] . catalogue-summary-first-record)
        ([C-end] . catalogue-summary-last-record)
        ("\C-s" . db-isearch-forward)
        ("\C-r" . db-isearch-backward)
        ("s" . catalogue-search)
        ("g" . catalogue-summary)
        ([return] . catalogue-edit)
        ("e" . catalogue-edit)
        ("a" . catalogue-add-item)
        ("/" . catalogue-disk-identify)
        ("I" . catalogue-disk-identify)
        ("B" . catalogue-borrow)
        ("L" . catalogue-lend)
        ("R" . catalogue-release)
        ("G" . catalogue-give-up)
        ("A" . catalogue-acquire)
        ("o" . catalogue-open-tray)
        ("\C-cr" . catalogue-reassign)
        ("r" . catalogue-report)
        ("m" . catalogue-summary-mark)
        ("u" . catalogue-summary-unmark)
        ("\C-cm" . catalogue-summary-mark-category)
        ("\C-cu" . catalogue-summary-unmark-category)
        ("U" . db-unmark-all)
        ("Fa" . catalogue-summary-filter-alien)
        ("Fb" . catalogue-summary-filter-borrowed)
        ("Fl" . catalogue-summary-filter-lended)
        ("Fn" . catalogue-summary-filter-native)
        ("\C-d" . catalogue-unregister)
        ("?" . describe-mode)
        ("q" . dbs-exit))
      do
      (define-key catalogue-summary-map (car binding) (cdr binding)))


;; Summary view menu:

(easy-menu-define nil catalogue-summary-map
  "Media catalogue summary view menu"
  '("Catalogue"
    ["Edit" catalogue-edit t]
    ["Add item" catalogue-add-item t]
    ["Delete" catalogue-unregister t]
    "-----"
    ["Search" catalogue-search t]
    "-----"
    ("Marking"
     ["Mark this item" catalogue-summary-mark t]
     ["Unmark this item" catalogue-summary-unmark t]
     ["Mark this category" catalogue-summary-mark-category t]
     ["Unmark this category" catalogue-summary-unmark-category t]
     ["Unmark all" db-unmark-all t])
    ("Filtering"
     ["Alien" catalogue-summary-filter-alien t]
     ["Native" catalogue-summary-filter-native t]
     ["Borrowed" catalogue-summary-filter-borrowed t]
     ["Lended" catalogue-summary-filter-lended t])
    "-----"
    ["Report" Catalogue-report t]
    "-----"
    ["Identify disk" catalogue-disk-identify t]
    ["Reassign disk" catalogue-reassign t]
    ["Open tray" catalogue-open-tray t]
    "-----"
    ["Borrow" catalogue-borrow t]
    ["Lend" catalogue-lend t]
    ["Release" catalogue-release t]
    ["Give up" catalogue-give-up t]
    ["Acquire" catalogue-acquire t]
    "-----"
    ["Quit summary" catalogue-exit t]))


;;; That's all.

(provide 'catalogue-summary)
