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


;;; Ensure correct load path:

(add-to-list 'load-path (directory-file-name (file-name-directory load-file-name)))


;;; Autoloads:

(autoload 'catalogue-view "catalogue-view" "View CD/DVD catalogue." t)
(autoload 'catalogue-disk-identify "catalogue-media" "Try to identify currently inserted disk in catalogue." t)


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

(defcustom catalogue-use-cd-text t
  "Make use of cd-text data if available when recognizing audio CD."
  :type 'boolean
  :group 'catalogue)

(defcustom catalogue-use-cdtool-database t
  "Make use of cdtool database when recognizing audio CD."
  :type 'boolean
  :group 'catalogue)


;;; Global key bindings:

(global-set-key "\C-cv" 'catalogue-view)
(global-set-key "\C-cd" 'catalogue-disk-identify)


;;; That's all.

(provide 'catalogue)
