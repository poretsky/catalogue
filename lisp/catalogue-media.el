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

(eval-when-compile
  (require 'cl)
  (require 'database)
  (require 'db-summary))

(require 'easymenu)
(require 'database)
(require 'catalogue)
(require 'catalogue-view)
(require 'catalogue-util)
(require 'catalogue-strings)


;;; Code:

(defconst catalogue-category-files-alist
  '((mp3-music . "\\.mp[23]$")
    (video-avi . "\\.avi$")
    (video-ogm . "\\.ogm$")
    (video-mpg . "\\.mpg$")
    (software-deb . "\\.deb")
    (software-rpm . "\\.rpms?$")
    (software-fbsd . "\\.tbz$")
    (software-ms . "\\.\\(exe\\|cab\\)$"))
  "Association list of disk categories and corresponding file masks
which should be used when guessing.")


;; Utility functions:

(defun catalogue-find-hole ()
  "Find non-complete item set and return draft of new record
or nil if no one is found."
  (declare (special first-link))
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

(defun catalogue-disk-info-extract (selector)
  "Extract value by selector regexp from cd-info output."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (and (re-search-forward selector nil t)
         (match-string 1))))

(defun catalogue-detect-media ()
  "Fetch media information and try to find it in the catalogue.
Return cons cell with found record index or nil in car
and record draft with filled id and media type in cdr.
For data disks the name is also preliminary set by the way."
  (declare (special first-link))
  (call-process "eject" nil nil nil "-t" catalogue-cd-dvd-device)
  (let ((draft (make-record dbc-database))
        (database dbc-database)
        (found nil))
    (with-temp-buffer
      (unless (zerop (call-process "cd-info" nil t nil
                                   "-q" "-I" "--no-header" "--dvd" "--no-device-info"
                                   "--iso9660" "-C" catalogue-cd-dvd-device))
        (error "No disk inserted or you have no access to %s" catalogue-cd-dvd-device))
      (record-set-field
       draft 'id
       (catalogue-disk-info-extract "^Audio CD, CDDB disc ID is +\\(.*\\)$")
       database)
      (record-set-field
       draft 'media
       (catalogue-disk-info-extract "^Disc mode is listed as: +\\(.*\\)")
       database)
      (unless (record-field draft 'id database)
        (let ((volume-id (catalogue-disk-info-extract "^ISO 9660: .* label +` *\\(.*?\\) *'$"))
              (listing nil))
          (unless volume-id
            (error "Unknown media format"))
          (goto-char (point-min))
          (while (re-search-forward "^ +-.* \\[LSN +\\([0-9]+\\)\\] +\\([0-9]+\\) " nil t)
            (let ((lsn (match-string 1))
                  (size (match-string 2)))
              (setq listing (cons (cons (string-to-number lsn) (concat lsn ":" size)) listing))))
          (record-set-field draft 'name volume-id database)
          (mapc
           (lambda (item)
             (setq volume-id (concat volume-id ";" (cdr item))))
           (sort listing
                 (lambda (x y)
                   (< (car x) (car y)))))
          (record-set-field draft 'id (md5 volume-id) database))))
    (maprecords
     (lambda (record)
       (when (string=
              (record-field draft 'id  database)
              (record-field record 'id database))
         (setq found maplinks-index)
         (maprecords-break)))
     database)
    (cons found draft)))

(defun catalogue-guess-cdda-info (draft)
  "Try to fetch some information for currently inserted audio CD using cdir
utility and fill name, category and description in the specified blank."
  (let ((database dbc-database))
    (with-temp-buffer
      (unless (zerop (call-process "cdir" nil t))
        (error "No audio disk inserted or you have no access to %s" catalogue-cd-dvd-device))
      (goto-char (point-min))
      (while (re-search-forward "^ +\\([0-9:.]+\\) +\\([0-9]+\\) +\\(.*\\)$" nil t)
        (replace-match "\\2. \\3 (\\1)"))
      (goto-char (point-min))
      (let* ((title (thing-at-point 'line))
             (breakpoint (or (string-match
                              " +- +\\([0-9]+:[0-9]+ +in +[0-9]+ +tracks\\)$"
                              title)
                             -1)))
        (record-set-field
         draft 'name
         (substring title 0 breakpoint)
         database)
        (record-set-field
         draft 'description
         (progn
           (end-of-line)
           (concat
            (if (< breakpoint 0)
                ""
              (match-string 1 title))
            (buffer-substring (point) (point-max))))
         database))))
  (record-set-field
   draft 'category
   (catalogue-language-string catalogue-category-names-alist 'music)
   dbc-database))

(defun catalogue-guess-data-disk-info (draft)
  "Try to guess category for a data disk and fill
category and description fields in specified blank.
And the name field also might be corrected."
  (let ((category 'misc)
        (description "")
        (title nil)
        (audio-content nil)
        (video-content nil)
        (sound-dir nil)
        (sound (float 0))
        (video (float 0))
        (total (float 0))
        (amount
         (mapcar
          (lambda (item)
            (cons (car item) (float 0)))
          catalogue-category-files-alist)))
    (with-temp-buffer
      (unless (zerop (call-process "iso-info" nil t nil
                                   "-l" catalogue-cd-dvd-device))
        (error "No data disk inserted or you have no access to %s" catalogue-cd-dvd-device))
      (setq title (catalogue-disk-info-extract "^Volume *: *\\(.*?\\) *$"))
      (unless (re-search-forward "^/:$" nil t)
        (error "Invalid disk"))
      (delete-region (point-min) (match-beginning 0))
      (goto-char (point-min))
      (let ((case-fold-search t)
            (root-subdir nil)
            (sound-subdir nil)
            (in-sound nil)
            (in-video nil))
        (if (re-search-forward "^/book/" nil t)
            (setq sound-dir (match-string 0))
          (do ((dirs '("sounds?" "songs" "audio" "music") (cdr dirs)))
              ((or (null dirs)
                   sound-dir))
            (let ((pattern (concat "^\\(/" (car dirs) "/[^/]+/\\):$")))
              (when (re-search-forward pattern nil t)
                (setq sound-dir (match-string 1)))
              (when (re-search-forward pattern nil t)
                (setq sound-dir nil)))))
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "^/\\([^/]+\\)/.*:$")
              (progn
                (setq root-subdir (match-string 1))
                (if (and sound-dir
                         (looking-at (concat "^" sound-dir "\\(\\([^/]+\\)/\\)?")))
                    (setq in-sound t
                          in-video nil
                          sound-subdir (match-string 2))
                  (if in-video
                      (setq in-video nil)
                    (if (looking-at "^/video_ts/:$")
                        (setq in-sound nil
                              in-video t)
                      (setq in-sound nil
                            in-video nil)))))
            (when (looking-at "^ +-.* \\[LSN +[0-9]+\\] +\\([0-9]+\\) +[^ ]+ +[0-9]+ +[0-9]+ +[0-9:]+ +\\(.*\\)$")
              (let ((fsize (float (string-to-number (match-string 1))))
                    (fname (match-string 2)))
                (mapc
                 (lambda (item)
                   (when (string-match (cdr item) fname)
                     (setcdr (assq (car item) amount)
                             (+ (cdr (assq (car item) amount))
                                fsize))
                     (if (eq 'mp3-music (car item))
                         (if (not in-sound)
                             (unless (or sound-dir
                                         (catalogue-string-empty-p root-subdir))
                               (add-to-list 'audio-content root-subdir))
                           (setq sound (+ sound fsize))
                           (unless (catalogue-string-empty-p sound-subdir)
                             (add-to-list 'audio-content sound-subdir)))
                       (when (or (eq 'video-avi (car item))
                                 (eq 'video-ogm (car item))
                                 (eq 'video-mpg (car item)))
                         (if (catalogue-string-empty-p root-subdir)
                             (when (string-match "^\\(.+\\)\\..+?$" fname)
                               (add-to-list 'video-content (match-string 1 fname)))
                           (add-to-list 'video-content root-subdir))))))
                 catalogue-category-files-alist)
                (when (and in-video (string-match ".*\\.\\(vob\\|ifo\\|bup\\)$" fname))
                  (setq video (+ video fsize)))
                (setq total (+ total fsize)))))
          (forward-line))))
    (unless (zerop total)
      (if (= total video)
          (setq category 'video-dvd)
        (let ((dominant (float 0)))
          (mapc
           (lambda (item)
             (when (> (cdr item) dominant)
               (setq category (car item)
                     dominant (cdr item))))
           amount))
        (when (eq category 'mp3-music)
          (when (and sound-dir
                     (>= (+ sound sound) total))
            (if (string-match "^/book/$" sound-dir)
                (setq category 'mp3-audiobook)
              (when (and (string-match "^/.*/\\(.*\\)/$" sound-dir)
                         (> (length (match-string 1 sound-dir))
                            (length title)))
                (setq title (match-string 1 sound-dir))))))
        (let ((content
               (cond
                ((eq 'mp3-music category)
                 audio-content)
                ((or (eq 'video-avi category)
                     (eq 'video-ogm category)
                     (eq 'video-mpg category))
                 video-content))))
          (when (and content (cdr content))
            (setq description
                  (concat
                   (catalogue-language-resource catalogue-toc-header)
                   "\n"))
            (mapc
             (lambda (item)
               (setq description (concat description item "\n")))
             (sort content 'string<))))))
    (when (> (length title)
             (length (record-field draft 'name dbc-database)))
      (record-set-field draft 'name title dbc-database))
    (record-set-field
     draft 'category
     (catalogue-language-string catalogue-category-names-alist category)
     dbc-database)
    (record-set-field draft 'description description dbc-database)))


;; Interactive commands:

(defun catalogue-disk-identify ()
  "Identify currently inserted disk if any and try to find it's index card.
This function is designed as an additional catalogue entry point,
so it pops up the database if necessary. For unknown disk the index card
draft is constructed and placed into the database as a new record,
but not committed. This draft can be further edited or deleted."
  (interactive)
  (let ((draft
         (cond
          ((db-data-display-buffer-p)
           (dbf-displayed-record))
          ((db-summary-buffer-p)
           (dbs-in-data-display-buffer (dbf-displayed-record)))
          (t (catalogue-db-open)
             nil)))
        (disk-info (catalogue-detect-media))
        (found nil))
    (if (car disk-info)
        (progn
          (setq catalogue-unknown-disk nil)
          (db-jump-to-record (car disk-info))
          (catalogue-summary-synch-position)
          (when (and (featurep 'emacspeak)
                     (interactive-p))
            (emacspeak-auditory-icon 'search-hit)
            (if (db-summary-buffer-p)
                (emacspeak-speak-line)
              (emacspeak-speak-current-window))))
      (if (record-field (setq disk-info (cdr disk-info)) 'name dbc-database)
          (catalogue-guess-data-disk-info disk-info)
        (catalogue-guess-cdda-info disk-info))
      (if draft
          (unless (setq found (catalogue-find-hole-in-item-set draft))
            (let ((hole (catalogue-find-hole)))
              (when hole
                (setq draft hole
                      found (record-field draft 'unit dbc-database)))))
        (setq draft (catalogue-find-hole)
              found (and draft (record-field draft 'unit dbc-database))))
      (setq catalogue-unknown-disk t)
      (if (db-data-display-buffer-p)
          (setq catalogue-restore-summary (dbf-summary-buffer))
        (dbs-exit)
        (setq catalogue-restore-summary t))
      (dbf-kill-summary)
      (if (not (catalogue-empty-p))
          (db-add-record)
        (db-add-record)
        (db-next-record 1)
        (db-delete-record t))
      (dbf-set-this-record-modified-p t)
      (if found
          (copy-record-to-record draft (dbf-displayed-record))
        (dbf-displayed-record-set-field 'set 1)
        (dbf-displayed-record-set-field
         'category (record-field disk-info 'category dbc-database))
        (dbf-displayed-record-set-field
         'name (record-field disk-info 'name dbc-database)))
      (unless (catalogue-string-empty-p (record-field disk-info 'description dbc-database))
        (dbf-displayed-record-set-field
         'description (record-field disk-info 'description dbc-database)))
      (dbf-displayed-record-set-field 'unit (or found 1))
      (dbf-displayed-record-set-field
       'media (record-field disk-info 'media dbc-database))
      (dbf-displayed-record-set-field-and-redisplay
       'id (record-field disk-info 'id dbc-database))
      (db-view-mode)
      (when (and (featurep 'emacspeak)
                 (interactive-p))
        (emacspeak-auditory-icon 'search-miss)
        (emacspeak-speak-current-window)))))

(defun catalogue-reassign ()
  "Reassign current record to the inserted disk."
  (interactive)
  (unless (or (db-data-display-buffer-p) (db-summary-buffer-p))
    (error "Not in data display or summary buffer"))
  (let ((disk-info (catalogue-detect-media)))
    (db-in-data-display-buffer
      (when (or (not (car disk-info))
                (yes-or-no-p "This disk is already registered. Are you sure? "))
        (when (car disk-info)
          (with-current-buffer (catalogue-operational-buffer)
            (let ((original-index dbc-index))
              (db-jump-to-record (car disk-info))
              (dbf-set-this-record-modified-p t)
              (dbf-displayed-record-set-field 'id catalogue-no-id)
              (db-jump-to-record original-index)))
          (catalogue-synchronize-with catalogue-operational-buffer))
        (dbf-set-this-record-modified-p t)
        (dbf-displayed-record-set-field
         'media (record-field (cdr disk-info) 'media dbc-database))
        (dbf-displayed-record-set-field-and-redisplay
         'id (record-field (cdr disk-info) 'id dbc-database))
        (db-accept-record)
        (db-view-mode)
        (db-save-database)
        (when (and (featurep 'emacspeak)
                   (interactive-p))
          (emacspeak-auditory-icon 'save-object))))))

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

(defun catalogue-open-tray ()
  "Open the disk tray."
  (interactive)
  (call-process "eject" nil 0 nil catalogue-cd-dvd-device))


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
