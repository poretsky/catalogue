;;; catalogue-startup.el --- CD/DVD collection holder bindings

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


;;; Ensure correct load path:

(add-to-list 'load-path (directory-file-name (file-name-directory load-file-name)))


;;; Autoloads:

(autoload 'catalogue-view "catalogue" "View CD/DVD catalogue." t)
(autoload 'catalogue-disk-identify "catalogue" "Try to identify currently inserted disk in catalogue." t)
(autoload 'catalogue-borrow "catalogue" "Register disk in catalogue as borrowed." t)
(autoload 'catalogue-lend "catalogue" "Register disk in catalogue as lended." t)
(autoload 'catalogue-release "catalogue" "Release borrowed or lended item." t)
(autoload 'catalogue-give-up "catalogue" "Register disk in catalogue as alien." t)
(autoload 'catalogue-reassign "catalogue" "Reassign current catalogue record to the inserted disk." t)
(autoload 'catalogue-unregister "catalogue" "Wipe disk id for current catalogue record." t)


;;; Key bindings:

(global-set-key "\C-cv" 'catalogue-view)
(global-set-key "\C-cd" 'catalogue-disk-identify)
(define-key database-view-mode-map "/" 'catalogue-disk-identify)
(define-key database-view-mode-map "B" 'catalogue-borrow)
(define-key database-view-mode-map "\C-cb" 'catalogue-borrow)
(define-key database-view-mode-map "L" 'catalogue-lend)
(define-key database-view-mode-map "\C-cl" 'catalogue-lend)
(define-key database-view-mode-map "R" 'catalogue-release)
(define-key database-view-mode-map "\C-cr" 'catalogue-release)
(define-key database-view-mode-map "G" 'catalogue-give-up)
(define-key database-view-mode-map "\C-cg" 'catalogue-give-up)
(define-key database-view-mode-map "\C-c\C-r" 'catalogue-reassign)
(define-key database-view-mode-map "\C-cu" 'catalogue-unregister)
