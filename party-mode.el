;;; party-mode.el --- Ain't no party like an Emacs party -*- lexical-binding: t -*-

;; Copyright © 2014 parkeristyping <parker.alford@gmail.com> 

;; Author: 
;; URL: https://github.com/parkeristyping/party-mode.el
;; Keywords: party, music, emms
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (emms "3.0") (async "1.6"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Usage:

;; M-x party-mode 
;; M-x stop-partying

;;; Code:

(setq party-mode-engaged-flag nil)
(setq party-mode-music-path (concat (file-name-directory #$) "music/crapface.mp3"))

(defun party-mode-random-face ()
  "Return the name of a random face from available faces."
  (nth (random (length (face-list))) (face-list)))

(defun party-mode-random-visible-buffer ()
  "Return a random visible buffer."
  (window-buffer (nth (random (length (window-list))) (window-list))))

(defun party-mode-update-random-buffer ()
  "Randomly update a visible buffer's face"
  (set-buffer (party-mode-random-visible-buffer))
  (buffer-face-set (party-mode-random-face)))

(defun party-mode-loop ()
  "Party loop"
  (party-mode-update-random-buffer)
  (async-start
   (lambda ()
     (sleep-for 0.05))
   (lambda (result)
     (if party-mode-engaged-flag
         (party-mode-loop)))))

(defun party-mode ()
  "Partaaaayyyy!"
  (interactive)
  (setq party-mode-engaged-flag 't)
  (emms-play-file party-mode-music-path)
  (party-mode-loop))

(defun stop-partying ()
  "Stop party mode and reset faces to defaults"
  (interactive)
  (setq party-mode-engaged-flag nil)
  (dolist (window (window-list))
    (set-buffer (window-buffer window))
    (buffer-face-set 'default))
  (emms-stop))

(provide 'party-mode)
;;; party-mode.el ends here
