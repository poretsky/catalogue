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

(autoload 'catalogue-disk-identify "catalogue-media" "Try to identify currently inserted disk in catalogue." t)
(autoload 'catalogue-reassign "catalogue-media" "Reassign current catalogue record to the inserted disk." t)

(autoload 'catalogue-borrow "catalogue-commands" "Register disk in catalogue as borrowed." t)
(autoload 'catalogue-lend "catalogue-commands" "Register disk in catalogue as lended." t)
(autoload 'catalogue-release "catalogue-commands" "Release borrowed or lended item." t)
(autoload 'catalogue-give-up "catalogue-commands" "Register disk in catalogue as alien." t)
(autoload 'catalogue-unregister "catalogue-commands" "Forget this disk forever." t)

(autoload 'catalogue-search "catalogue-search" "Search record by specified field and pattern." t)


;;; Global key bindings:

(global-set-key "\C-cv" 'catalogue-view)
(global-set-key "\C-cd" 'catalogue-disk-identify)
