;;; catalogue-strings.el --- Common hardcoded string resources

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


;;; Code:

(defconst catalogue-category-names-alist
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

(defconst catalogue-media-types-alist
  '((audio-cd . "Audio CD")
    (data-cd . "Data CD")
    (dvd . "DVD"))
  "Detectable media type names alist.")


;;; That's all.

(provide 'catalogue-strings)
