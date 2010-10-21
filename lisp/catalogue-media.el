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
(require 'easymenu)
(require 'database)
(require 'catalogue)
(require 'catalogue-view)
(require 'catalogue-util)
(require 'catalogue-strings)


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

(defconst catalogue-mp3-contents-title
  '(("en" . "Albums:")
    ("ru" . "Альбомы:"))
  "Title strings for mp3 disk content description for supported languages.")


(defun catalogue-find-hole ()
  "Find non-complete item set and return draft of new record
or nil if no one is found."
  (let ((draft nil)
        (name "")
        (category ""))
    (maprecords
     (lambda (record)
       (let ((hole
              (and (> (record-field record 'set dbc-database) 1)
                   (not (string= name (record-field record 'name dbc-database)))
                   (not (string= category (record-field record 'category dbc-database)))
                   (catalogue-find-hole-in-item-set record))))
         (if (not hole)
             (setq name (record-field record 'name dbc-database)
                   category (record-field record 'category dbc-database))
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
  (or (cdr (assq category (cdr (assoc (catalogue-language) catalogue-category-names-alist))))
      (symbol-name category)))

(defun catalogue-media-type (media)
  "Return name of given media type."
  (or (cdr (assq media catalogue-media-types-alist))
      (symbol-name media)))

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
  "Identify currently inserted disk if any and return it's index or nil.
This function is designed as an additional catalogue entry point,
so it pops up the database if necessary. For unknown disk the index card
draft is constructed and placed into the database as a new record,
but not committed. This draft can be further edited or deleted.
Being called non-interactively this function does not affect
catalogue database display in any way."
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
                          (catalogue-media-type 'dvd)
                        (catalogue-media-type 'data-cd))
                    (catalogue-media-type 'audio-cd)))
           (id (md5 (if data
                        (let ((coding-system-for-read 'raw-text))
                          (shell-command-to-string
                           (concat "find 2>/dev/null "
                                   catalogue-cd-dvd-mountpoint
                                   " -printf \"%A@%C@%T@%U%G%m%n%s%P\"")))
                      id)
                    nil nil 'raw-text t))
           (draft
            (cond
             ((db-data-display-buffer-p)
              (dbf-displayed-record))
             ((db-summary-buffer-p)
              (dbs-in-data-display-buffer (dbf-displayed-record)))
             (t (catalogue-db-open)
                nil)))
           (category (if data
                         (catalogue-guess-disk-category)
                       'music))
           (cdinfo (if data
                       (when (eq category 'mp3-music)
                         (catalogue-guess-mp3-disk-info))
                     (catalogue-guess-audio-disk-info)))
           (found nil))
      (when data
        (shell-command-to-string (concat "umount "
                                         catalogue-cd-dvd-mountpoint)))
      (maprecords
       (lambda (record)
         (when (string= id
                        (record-field record 'id dbc-database))
           (setq found maplinks-index)
           (maprecords-break)))
       dbc-database)
      (if found
          (progn
            (when (interactive-p)
              (setq catalogue-unknown-disk nil)
              (db-jump-to-record found)
              (catalogue-summary-synch-position)
              (when (featurep 'emacspeak)
                (emacspeak-auditory-icon 'search-hit)
                (if (db-summary-buffer-p)
                    (emacspeak-speak-line)
                  (emacspeak-speak-current-window))))
            found)
        (if draft
            (unless (setq found (catalogue-find-hole-in-item-set draft))
              (let ((hole (catalogue-find-hole)))
                (when hole
                  (setq draft hole)
                  (setq found (record-field draft 'unit dbc-database)))))
          (setq draft (catalogue-find-hole))
          (setq found (and draft (record-field draft 'unit dbc-database))))
        (when (interactive-p)
          (setq catalogue-unknown-disk t)
          (if (db-summary-buffer-p)
              (progn
                (dbs-exit)
                (setq catalogue-restore-summary t))
            (setq catalogue-restore-summary (dbf-summary-buffer)))
          (dbf-kill-summary))
        (db-in-data-display-buffer
         (if (not (catalogue-empty-p))
             (db-add-record)
           (db-add-record)
           (db-next-record 1)
           (db-delete-record t))
         (dbf-set-this-record-modified-p t)
         (if found
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
         (dbf-displayed-record-set-field 'unit (or found 1))
         (dbf-displayed-record-set-field 'media media)
         (if (not (interactive-p))
             (dbf-displayed-record-set-field 'id id)
           (dbf-displayed-record-set-field-and-redisplay 'id id)
           (db-view-mode)))
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (emacspeak-auditory-icon 'search-miss)
          (emacspeak-speak-current-window))
        nil))))

(defun catalogue-reassign ()
  "Reassign current record to the inserted disk."
  (interactive)
  (unless (or (db-data-display-buffer-p) (db-summary-buffer-p))
    (error "Not in data display or summary buffer"))
  (if (catalogue-disk-identify)
      (progn
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (emacspeak-auditory-icon 'warn-user))
        (message "Already registered disk"))
    (db-in-data-display-buffer
     (let ((new-id (dbf-displayed-record-field 'id)))
       (db-delete-record t)
       (dbf-set-this-record-modified-p t)
       (dbf-displayed-record-set-field-and-redisplay 'id new-id)
       (db-accept-record)
       (db-view-mode)
       (db-save-database)))
    (when (and (featurep 'emacspeak)
               (interactive-p))
      (emacspeak-auditory-icon 'save-object))
    (message "Successfully reassigned")))

(defun catalogue-cancel-registration ()
  "Cancel new disk registration."
  (interactive)
  (unless (db-data-display-buffer-p)
    (error "Not in data display buffer"))
  (unless (eq dbf-minor-mode 'view)
    (error "Not in viewing mode"))
  (unless catalogue-unknown-disk
    (error "Not a new disk"))
  (catalogue-delete)
  (db-save-database)
  (use-local-map catalogue-view-map)
  (cond
   ((eq catalogue-restore-summary t)
    (setq catalogue-restore-summary nil)
    (db-summary))
   (catalogue-restore-summary
    (setq catalogue-restore-summary nil)
    (save-selected-window
      (db-summary)))
   (t nil))
  (when (and (featurep 'emacspeak)
             (interactive-p))
    (emacspeak-auditory-icon 'close-object)))


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


;;; That's all.

(provide 'catalogue-media)
