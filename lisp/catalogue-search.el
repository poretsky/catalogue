;;; catalogue-search.el --- CD/DVD catalogue search engine

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
(require 'database)
(require 'catalogue)


;;; Code:

(defvar catalogue-searchable-fields-list nil
  "Remembers list of searchable fields.")

(defvar catalogue-search-field-history nil
  "Searched field name history.")


(defun catalogue-searchable-fields ()
  "Get list of searchable fields."
  (unless catalogue-searchable-fields-list
    (let ((catalogue-searching-p t))
      (db-next-record 0)
      (db-last-field)
      (setq catalogue-searchable-fields-list
            (do ((result nil (nconc result (list (symbol-name (dbf-this-field-name)))))
                 (i 0 (1+ i)))
                ((>= i dbf-displayspecs-length)
                 result)
              (db-next-field 1))))
    (db-view-mode)
    (db-next-record 0))
  catalogue-searchable-fields-list)


;;; Interactive commands:

(defun catalogue-search (field pattern)
  "Search record by specified field and pattern."
  (interactive
   (list
    (completing-read "Choose field to search by: "
                     (catalogue-searchable-fields)
                     nil t nil
                     'catalogue-search-field-history
                     (car (catalogue-searchable-fields)))
    (read-string "Enter search pattern: ")))
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when catalogue-editing-p
    (error "This operation is only available in view mode"))
  (when dbc-database-modified-p
    (error "Database is modified and not saved"))
  (when (string= pattern "")
    (error "Empty pattern is not allowed"))
  (let ((catalogue-searching-p t))
    (db-next-record 0)
    (db-first-field)
    (do ((fields (catalogue-searchable-fields) (cdr fields)))
        ((or (null fields)
             (string= field (car fields))))
      (db-next-field 1))
    (db-search-field pattern))
  (db-view-mode)
  (db-next-record 0)
  (when (and (featurep 'emacspeak)
	     (interactive-p))
    (emacspeak-auditory-icon 'search-hit)))


;;; That's all.

(provide 'catalogue-search)
