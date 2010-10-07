;;; catalogue-media.el --- Catalogue disk identification functions

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
(require 'catalogue-keymap)


;;; Code:

(defconst catalogue-find-subdirs-options
  " -maxdepth 1 -mindepth 1 -noleaf -type d "
  "Options passed to find utility to locate subdirectories.")

(defconst catalogue-category-files-alist
  '((mp3-music . ".*\\.mp[23]")
    (video-avi . ".*\\.avi")
    (video-ogm . ".*\\.ogm")
    (video-mpg . ".*\\.mpg")
    (software-deb . ".*\\.deb")
    (software-rpm . ".*\\.rpm")
    (software-fbsd . ".*\\.tbz")
    (software-ms . ".*\\.exe"))
  "Association list of disk categories and corresponding file masks
which should be used when guessing.")

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

(defconst catalogue-mp3-contents-title
  '(("en" . "Albums:")
    ("ru" . "Альбомы:"))
  "Title strings for mp3 disk content description for supported languages.")


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
      (cons (substring title 0 breakpoint)
             (progn (end-of-line)
                    (concat (if (< breakpoint 0)
                                ""
                              (match-string 1 title))
                            (buffer-substring (point) (point-max))))))))

(defun catalogue-find-sole-subdir (dir)
  "Check if given directory contains only one subdir
and return it's name or nil otherwise."
  (let ((coding-system-for-read catalogue-media-coding-system))
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
                  "-printf \"%f\"")))))

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
       (let* ((name (and dirs
                         (catalogue-find-sole-subdir (car dirs))))
              (coding-system-for-read catalogue-media-coding-system)
              (content
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
                 "-printf \"%f\\n\""))))
         (cons
          (or name "")
          (if (string= content "")
              content
            (concat
             (cdr (assoc (catalogue-language) catalogue-mp3-contents-title))
             "\n"
             content)))))))

(defun catalogue-count-files-amount (fmask)
  "Return amount of disk space occupied by files defined by given mask
on CD/DVD. The mask should be a regexp matching full
pathname of a file relative to the CD/DVD mountpoint.
Matching is done case insensitive."
  (let ((amount (float 0))
        (lang-orig (getenv "LANG")))
    (with-temp-buffer
      (insert "(setq amount (+ amount))")
      (backward-char 2)
      (when catalogue-media-coding-system
        (setenv "LANG" (symbol-name catalogue-media-coding-system)))
      (call-process "find" nil (list (current-buffer) nil) nil
                    catalogue-cd-dvd-mountpoint
                    "-noleaf"
                    "-type" "f"
                    "-iregex"
                    (expand-file-name fmask
                                      catalogue-cd-dvd-mountpoint)
                    "-printf" " %s")
      (when catalogue-media-coding-system
        (setenv "LANG" lang-orig))
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
  (or (cdr (assoc category (cdr (assoc (catalogue-language) catalogue-category-names))))
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
                        (let ((coding-system-for-read 'raw-text))
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
        (if draft
            (unless (setq index (catalogue-find-hole-in-disk-set
                                 (record-field draft 'name dbc-database)))
              (let ((hole (catalogue-find-hole)))
                (when hole
                  (setq draft hole)
                  (setq index (record-field draft 'unit dbc-database)))))
          (setq draft (catalogue-find-hole))
          (setq index (and draft (record-field draft 'unit dbc-database))))
        (let ((catalogue-editing-p t))
          (if (not (catalogue-empty-p))
              (db-add-record)
            (db-add-record)
            (db-next-record 1)
            (db-delete-record t)))
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
        (dbf-displayed-record-set-field 'id id)
        (setq catalogue-unknown-disk t)
        (db-view-mode)
        (dbf-redisplay-entire-record-maybe)
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (emacspeak-auditory-icon 'search-miss))))))

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

(defun catalogue-cancel-registration ()
  "Cancel new disk registration."
  (interactive)
  (unless (eq major-mode 'database-mode)
    (error "This operation can only be done from the database mode"))
  (when catalogue-editing-p
    (error "Not in viewing mode"))
  (unless catalogue-unknown-disk
    (error "Not a new disk"))
  (catalogue-delete-record)
  (db-save-database)
  (use-local-map catalogue-view-map)
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))


;;; That's all.

(provide 'catalogue-media)
