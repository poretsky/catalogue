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

(defcustom catalogue-auto-sort t
  "*If true records will be automatically sorted after editing."
  :type 'boolean
  :group 'catalogue)

(defcustom catalogue-auto-commit t
  "*If true the record will be automatically committed
after editing or performing actions like
`borrow', `lend', `release' or `give-up'."
  :type 'boolean
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

(defvar catalogue-display-format-path nil
  "Path list to the format files for various display modes.")

(defconst catalogue-find-subdirs-options
  " -maxdepth 1 -mindepth 1 -noleaf -type d "
  "Options passed to find utility to locate subdirectories.")

(defconst catalogue-no-id "noid"
  "Dummy disk identifier.")

(defvar catalogue-mp3-contents-title "Albums:"
  "Title string for mp3 disk content description.")

(defvar catalogue-date-format "%B %e, %Y"
  "Date display format.")

(defvar catalogue-category-names-alist
  '((music)
    (mp3-music)
    (mp3-audiobook)
    (video-dvd)
    (misc))
  "Association list of disk categories and it's names.")

(defvar catalogue-category-files-alist nil
  "Association list of disk categories and corresponding file masks
which should be used when guessing.")

(defvar catalogue-unknown-disk nil
  "Indicates whether this disk has to be registered.")

(defvar catalogue-editing-p nil
  "Edit mode indicator.")

(defvar catalogue-empty-p nil
  "Catalogue emptiness indicator.")

(defconst catalogue-category-names
  '(("en"
     (music . "Music")
     (mp3-music . "Music (mp3)")
     (mp3-audiobooks . "Audiobook (mp3)")
     (video-dvd . "Films (DVD)")
     (video-avi . "Films (avi)")
     (video-ogm . "Films (ogm)")
     (video-mpg . "Films (mpg)")
     (software-deb . "Software (Debian Linux)")
     (software-rpm . "Software (RH Linux)")
     (software-fbsd . "Software (FreeBSD)")
     (software-ms . "Software (MSDOS/Windows)")
     (misc . "Miscellaneous"))
    ("ru"
     (music . "Музыка")
     (mp3-music . "Музыка (mp3)")
     (mp3-audiobook . "Аудиокниги (mp3)")
     (video-dvd . "Фильмы (DVD)")
     (video-avi . "Фильмы (avi)")
     (video-ogm . "Фильмы (ogm)")
     (video-mpg . "Фильмы (mpg)")
     (software-deb . "Software (Debian Linux)")
     (software-rpm . "Software (RH Linux)")
     (software-fbsd . "Software (FreeBSD)")
     (software-ms . "Software (MSDOS/Windows)")
     (misc . "Разное")))
  "Category names by language.")

(defun catalogue-db-open ()
  "Open existing user database or create a fresh one."
  (unless (file-exists-p catalogue-resource-directory)
    (make-directory catalogue-resource-directory))
  (let ((db-format-file-path catalogue-shared-resource-path)
        (db-aux-file-path catalogue-shared-resource-path))
    (db-find-file catalogue-db-file)
    (use-local-map catalogue-view-map)
    (unless (file-exists-p catalogue-db-file)
      (db-toggle-internal-file-layout t))))

(defun catalogue-language ()
  "get current database language as a valid two-letter code."
  (or catalogue-database-language
      (let ((lang (or (getenv "LANG") "")))
        (cond
         ((string-match "^ru_RU" lang) "ru")
         (t "en")))))

(defun catalogue-edit-setup ()
  "Setup record editing mode."
  (use-local-map catalogue-edit-map))

(defun catalogue-view-setup ()
  "Setup view mode."
  (if catalogue-unknown-disk
      (use-local-map catalogue-preview-map)
    (use-local-map catalogue-view-map)))

(defun catalogue-choose-display-format (record)
  "Choose an appropriate display format for the record."
  (let ((db-format-file-path catalogue-display-format-path))
    (cond
     ((and catalogue-empty-p
           (or (null (record-field record 'id dbc-database))
               (string= (record-field record 'id dbc-database) "")))
      (db-change-format "empty"))
     (catalogue-editing-p
      (db-change-format "edit disk info"))
     (catalogue-unknown-disk
      (db-change-format "disk registration form"))
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

(defun catalogue-check-emptiness ()
  "Check if the catalogue database is empty and set `catalogue-empty-p'.
Should be called when reading the database."
  (setq catalogue-empty-p t)
  (maprecords
   (lambda (record)
     (when (and (record-field record 'id database)
		(not (string= "" (record-field record 'id database))))
       (setq catalogue-empty-p nil)
       (maprecords-break)))
   database))

(defun catalogue-initialize-record (record database)
  "Initialize newly created record."
  (record-set-field record 'id catalogue-no-id database))

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
  (setq catalogue-unknown-disk nil)
  (when catalogue-empty-p
    (setq catalogue-empty-p nil)
    (db-next-record 1)
    (db-delete-record t)))

(defun catalogue-find-hole ()
  "Find non-complete disk set and return draft of new record
or nil if no one is found."
  (let ((draft nil)
	(skip ""))
    (maprecords
     (lambda (record)
       (let ((hole (and (> (record-field record 'set dbc-database) 1)
			(not (string= skip
				      (record-field record 'name
						    dbc-database)))
			(catalogue-find-hole-in-disk-set
			 (record-field record 'name dbc-database)))))
	 (if (not hole)
	     (setq skip (record-field record 'name dbc-database))
	   (setq draft (copy-record record))
	   (record-set-field draft 'unit hole dbc-database)
	   (maprecords-break))))
     dbc-database)
    draft))

(defun catalogue-guess-audio-disk-info ()
  "Query local CDDB and return cons cell
with the disk name in car and description in cdr."
  (with-temp-buffer
    (unless (= 0 (call-process "cdir" nil t))
      (error "No disk inserted"))
    (goto-char (point-min))
    (replace-regexp "^ +\\([0-9:.]+\\) +\\([0-9]+\\) +\\(.*\\)$"
		    "\\2. \\3 (\\1)")
    (goto-char (point-min))
    (let* ((title (thing-at-point 'line))
	   (breakpoint (or (string-match
			    " +- +\\([0-9]+:[0-9]+ +in +[0-9]+ +tracks\\)$"
			    title)
			   -1)))
      (nconc (list (substring title 0 breakpoint))
	     (progn (end-of-line)
		    (concat (if (< breakpoint 0)
				""
			      (match-string 1 title))
			    (buffer-substring (point) (point-max))))))))

(defun catalogue-find-sole-subdir (dir)
  "Check if given directory contains only one subdir
and return it's name or nil otherwise."
  (and (= 1 (length
	     (shell-command-to-string
	      (concat "find "
		      (expand-file-name dir catalogue-cd-dvd-mountpoint)
		      catalogue-find-subdirs-options
		      "-printf \".\""))))
       (shell-command-to-string
	(concat "find "
		(expand-file-name dir catalogue-cd-dvd-mountpoint)
		catalogue-find-subdirs-options
		"-printf \"%f\""))))

(defun catalogue-guess-mp3-disk-info ()
  "Return cons cell with guessed name in car and description in cdr."
  (do ((dirs '("sound" "Sound" "SOUND"
	       "sounds" "Sounds" "SOUNDS"
	       "audio" "Audio" "AUDIO"
	       "music" "Music" "MUSIC"
	       "songs" "Songs" "SONGS")
	     (cdr dirs)))
      ((or (null dirs)
	   (file-directory-p (expand-file-name (car dirs)
					       catalogue-cd-dvd-mountpoint)))
       (let ((name (and dirs
			(catalogue-find-sole-subdir (car dirs)))))
	 (nconc
	  (list (or name ""))
	  (concat
	   catalogue-mp3-contents-title
	   "\n"
	   (shell-command-to-string
	    (concat
	     "find \""
	     (if name
		 (expand-file-name name
				   (expand-file-name
				    (car dirs)
				    catalogue-cd-dvd-mountpoint))
	       (if dirs
		   (expand-file-name (car dirs)
				     catalogue-cd-dvd-mountpoint)
		 catalogue-cd-dvd-mountpoint))
	     "\""
	     catalogue-find-subdirs-options
	     "-printf \"%f\\n\""))))))))

(defun catalogue-count-files-amount (fmask)
  "Return amount of disk space occupied by files defined by given mask
on CD/DVD. The mask should be a regexp matching full
pathname of a file relative to the CD/DVD mountpoint.
Matching is done case insensitive."
  (let ((amount (float 0)))
    (with-temp-buffer
      (insert "(setq amount (+ amount))")
      (backward-char 2)
      (call-process "find" nil (list (current-buffer) nil) nil
		    catalogue-cd-dvd-mountpoint
		    "-noleaf"
		    "-type" "f"
		    "-iregex"
		    (expand-file-name fmask
				      catalogue-cd-dvd-mountpoint)
		    "-printf" " %s")
      (eval-buffer))
    amount))

(defun catalogue-video-dvd-p ()
  "Return t if the mounted disk is recognized as video DVD."
  (= (catalogue-count-files-amount ".*")
     (catalogue-count-files-amount "video_ts/.*\\.\\(vob\\|ifo\\|bup\\)")))

(defun catalogue-audio-book-p ()
  "Try to recognize the mounted CD as an audio book
and return t if success."
  (> (catalogue-count-files-amount "book/.*\\.mp3")
     0))

(defun catalogue-category-name (category)
  "Return name of given category."
  (or (cdr (assoc category catalogue-category-names-alist))
      (symbol-name category)))

(defun catalogue-guess-disk-category ()
  "Try to guess category of the inserted disk."
  (cond
   ((catalogue-video-dvd-p)
    'video-dvd)
   ((catalogue-audio-book-p)
    'mp3-audiobook)
   (t (let ((total (catalogue-count-files-amount ".*")))
	(when (= 0 total)
	  (error "No files on this disk"))
	(do ((cfl catalogue-category-files-alist (cdr cfl)))
	    ((or (null cfl)
		 (> (/ (catalogue-count-files-amount (cdr (car cfl))) total)
		    catalogue-meaningful-files-quota))
	     (if cfl
		 (car (car cfl))
	       'misc)))))))

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

(defun catalogue-disk-identify ()
  "Identify currently inserted disk if any."
  (interactive)
  (shell-command-to-string
   (concat "eject -t "
	   catalogue-cd-dvd-device))
  (let ((id (shell-command-to-string (concat "cd-discid "
					     catalogue-cd-dvd-device
					     " 2>/dev/null"))))
    (when (not (string-match "[^ ]+" id))
      (error "No disk inserted"))
    (shell-command-to-string (concat "umount " catalogue-cd-dvd-mountpoint))
    (let* ((data (= 0 (call-process "mount"
				    nil nil nil
				    catalogue-cd-dvd-mountpoint)))
	   (media (if data
		      (if (string= "023bfd01" (match-string 0 id))
			  "DVD"
			"Data CD")
		    "Audio CD"))
	   (id (md5 (if data
			(let ((process-coding-system-alist
			       '((".*" . raw-text))))
			  (shell-command-to-string
			   (concat "find 2>/dev/null "
				   catalogue-cd-dvd-mountpoint
				   " -printf \"%A@%C@%T@%U%G%m%n%s%P\"")))
		      id)
		    nil nil 'raw-text t))
	   (draft (if (eq major-mode 'database-mode)
		      (dbf-displayed-record)
		    (catalogue-db-open)
		    nil))
	   (category (if data
			 (catalogue-guess-disk-category)
		       'music))
	   (cdinfo (if data
		       (when (eq category 'mp3-music)
			 (catalogue-guess-mp3-disk-info))
		     (catalogue-guess-audio-disk-info)))
	   (index 0)
	   (found nil))
      (when data
	(shell-command-to-string (concat "umount "
					 catalogue-cd-dvd-mountpoint)))
      (maprecords
       (lambda (record)
	 (setq index (1+ index))
	 (when (string= id
			(record-field record 'id dbc-database))
	   (setq found t)
	   (maprecords-break)))
       dbc-database)
      (if found
	  (progn (setq catalogue-unknown-disk nil)
		 (db-jump-to-record index)
		 (when (and (featurep 'emacspeak)
			    (interactive-p))
		   (emacspeak-auditory-icon 'search-hit)))
	(setq catalogue-unknown-disk t)
	(if draft
	    (unless (setq index (catalogue-find-hole-in-disk-set
				 (record-field draft 'name dbc-database)))
	      (let ((hole (catalogue-find-hole)))
		(when hole
		  (setq draft hole)
		  (setq index (record-field draft 'unit dbc-database)))))
	  (setq draft (catalogue-find-hole))
	  (setq index (and draft (record-field draft 'unit dbc-database))))
	(db-add-record)
	(dbf-set-this-record-modified-p t)
	(if index
	    (copy-record-to-record draft (dbf-displayed-record))
	  (dbf-displayed-record-set-field 'set 1)
	  (dbf-displayed-record-set-field 'category
					  (catalogue-category-name category))
	  (when cdinfo
	    (dbf-displayed-record-set-field 'name (car cdinfo))))
	(when (and cdinfo
		   (string-match (catalogue-category-name category)
				 (dbf-displayed-record-field 'category)))
	  (dbf-displayed-record-set-field 'description (cdr cdinfo)))
	(dbf-displayed-record-set-field 'unit (or index 1))
	(dbf-displayed-record-set-field 'media media)
	(dbf-displayed-record-set-field-and-redisplay 'id id)
	(db-view-mode)
	(when (and (featurep 'emacspeak)
		   (interactive-p))
	  (emacspeak-auditory-icon 'search-miss))))))

(defun catalogue-edit ()
  "Set display format convenient for editing."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when catalogue-empty-p
    (error "Catalogue is empty"))
  (setq catalogue-editing-p t)
  (db-next-record 0)
  (db-first-field)
  (setq catalogue-editing-p t)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defun catalogue-view ()
  "Enter the disks catalogue."
  (interactive)
  (setq catalogue-unknown-disk nil)
  (catalogue-db-open)
  (when (and (featurep 'emacspeak)
	     (interactive-p))
    (emacspeak-auditory-icon 'open-object)))

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

(defun catalogue-reassign ()
  "Reassign current record to the inserted disk."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (catalogue-disk-identify)
  (if catalogue-unknown-disk
      (let ((new-id (dbf-displayed-record-field 'id)))
	(db-delete-record t)
	(dbf-set-this-record-modified-p t)
	(dbf-displayed-record-set-field-and-redisplay 'id new-id)
	(db-accept-record)
	(when (and (featurep 'emacspeak)
		   (interactive-p))
	  (emacspeak-auditory-icon 'select-object))
	(message "Successfully reassigned"))
    (when (and (featurep 'emacspeak)
	       (interactive-p))
      (emacspeak-auditory-icon 'search-hit))
    (message "Already registered disk")))

(defun catalogue-unregister ()
  "Wipe disk id for current record."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (dbf-set-this-record-modified-p t)
  (dbf-displayed-record-set-field-and-redisplay 'id catalogue-no-id)
  (when (and (featurep 'emacspeak)
	     (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun catalogue-commit ()
  "Commit current record to the database after editing."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (unless catalogue-editing-p
    (error "Not in editing mode"))
  (setq catalogue-editing-p nil)
  (when catalogue-auto-sort
    (db-sort t))
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
  (unless catalogue-editing-p
    (error "Not in editing mode"))
  (setq catalogue-editing-p nil)
  (db-revert-database)
  (db-view-mode)
  (db-next-record 0)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun catalogue-cancel-registration ()
  "Cancel new disk registration."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when catalogue-editing-p
    (error "Not in viewing mode"))
  (unless catalogue-unknown-disk
    (error "Not a new disk"))
  (db-delete-record t)
  (db-save-database)
  (use-local-map catalogue-view-map)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))


;;; That's all.

(provide 'catalogue)
