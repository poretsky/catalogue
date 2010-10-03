;;; catalogue-keymap.el --- Key bindings for catalogue buffers

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
(require 'easymenu)


;;; Code:

;; Key bindings for catalogue viewing:
(defvar catalogue-view-map (make-keymap)
  "Catalogue view mode keymap.")
(suppress-keymap catalogue-view-map t)
(loop for binding in 
      '(("/" . catalogue-disk-identify)
        ([return] . catalogue-edit)
        ("e" . catalogue-edit)
        ("B" . catalogue-borrow)
        ("\C-cb" . catalogue-borrow)
        ("L" . catalogue-lend)
        ("\C-cl" . catalogue-lend)
        ("R" . catalogue-release)
        ("\C-cr" . catalogue-release)
        ("G" . catalogue-give-up)
        ("\C-cg" . catalogue-give-up)
        ("\C-c\C-r" . catalogue-reassign)
        ("U" . catalogue-unregister)
        ("\C-cu" . catalogue-unregister)
        ("?" . describe-mode)
        ("n" . db-next-record)
        ([next] . db-next-record)
        ("p" . db-previous-record)
        ([prior] . db-previous-record)
        ("q" . db-exit))
      do
      (define-key catalogue-view-map (car binding) (cdr binding)))

;; Catalogue view menu:
(easy-menu-define nil catalogue-view-map
  "Media catalogue view menu"
  '("Catalogue"
    ["Edit" catalogue-edit (not catalogue-empty-p)]
    ["Identify disk" catalogue-disk-identify t]
    ["Borrow" catalogue-borrow (not (or catalogue-empty-p catalogue-unknown-disk (catalogue-native-p) (catalogue-borrowed-p)))]
    ["Lend" catalogue-lend (not (or catalogue-empty-p catalogue-unknown-disk))]
    ["Release" catalogue-release (not (or catalogue-empty-p catalogue-unknown-disk))]
    ["Give up" catalogue-give-up (not (or catalogue-empty-p catalogue-unknown-disk))]
    ["Reassign" catalogue-reassign (not (or catalogue-empty-p catalogue-unknown-disk))]
    ["Quit" db-exit t]))


;; Key bindings for new disk registration preview:
(defvar catalogue-preview-map (make-keymap)
  "New disk registration preview mode keymap.")
(suppress-keymap catalogue-preview-map t)
(loop for binding in
      '(("e" . catalogue-edit)
        ([return] . catalogue-edit)
        ("q" . catalogue-cancel-registration))
      do
      (define-key catalogue-preview-map (car binding) (cdr binding)))

;; New disk registration preview menu:
(easy-menu-define nil catalogue-preview-map
  "New disk registration preview menu"
  '("Catalogue"
    ["Edit" catalogue-edit t]
    ["Cancel" catalogue-cancel-registration t]))


;; Key bindings for catalogue editing:
(defvar catalogue-edit-map (make-keymap)
  "Catalogue edit mode keymap.")
(loop for binding in
      '(("\C-c\C-c" . catalogue-commit)
        ("\C-cq" . catalogue-cancel)
        ("\C-xU" . db-revert-field)
        ([next] . db-next-record)
        ([prior] . db-previous-record)
        ("\C-n" . db-next-line-or-field)
        ([down] . db-next-line-or-field)
        ("\C-p" . db-previous-line-or-field)
        ([up] . db-previous-line-or-field)
        ("\C-f" . db-forward-char)
        ([right] . db-forward-char)
        ("\C-b" . db-backward-char)
        ([left] . db-backward-char)
        ("\M-f" . db-forward-word)
        ("\M-b" . db-backward-word)
        ("\C-a" . db-beginning-of-line-or-field)
        ([home] . db-beginning-of-line-or-field)
        ([end] . db-end-of-line-or-field)
        ("\r" . db-newline)
        ("\n" . db-newline)
        ("\C-o" . db-open-line)
        ("\C-d" . db-delete-char)
        ("\177" . db-backward-delete-char)
        ("\M-d" . db-kill-word)
        ("\M-\177" . db-kill-word)
        ("\C-k" . db-kill-line)
        ("\M-k" . db-kill-to-end)
        ("\C-w" . db-kill-region)
        ("\M-w" . db-copy-region-as-kill)
        ("\C-s" . db-isearch-forward)
        ("\C-r" . db-isearch-backward)
        ("\M-?" . db-field-help))
      do
      (define-key catalogue-edit-map (car binding) (cdr binding)))

;; Catalogue edit menu:
(easy-menu-define nil catalogue-edit-map
  "Media catalogue edit menu"
  '("Catalogue"
    ["Commit" catalogue-commit t]
    ["Cancel" catalogue-cancel t]
    ["Revert record" db-revert-record t]
    ["Revert field" db-revert-field t]
    ["Help on field" db-field-help t]))


;;; That's all.

(provide 'catalogue-keymap)
