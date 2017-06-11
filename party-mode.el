;;; party-mode.el --- Ain't no party like an Emacs party -*- lexical-binding: t -*-

;; Copyright © 2017 parkeristyping <parker.alford@gmail.com>

;; Author: Parker Lawrence
;; URL: https://github.com/parkeristyping/party-mode.el
;; Keywords: party, music, emms
;; Version: 0.2.0
;; Package-Requires: ((emacs "24") (async "1.6"))

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
(defvar party-mode-engaged-flag nil
  "Variable indicating whether or not party-mode is currently engaged. Should not be customized.")
(defcustom party-mode-start-music-fn nil
  "Variable containing a function to start music playback when party-mode is engaged.
If value is `nil`, no music playback will be triggered.

If you customize this variable, you'll want to make a corresponding customization
to `party-mode-stop-music-fn` so that playback stops when you are done partying.

Here are a couple examples of how you can customize this variable:
  ;; Use EMMS to stream a song from the web (EMMS must be configured for this to work)
  (setq party-mode-start-music-fn
    (lambda () (emms-play-url \"https://s3-us-west-2.amazonaws.com/partymode/party_music.mp3\")))
  ;; Or, maybe you want to start a song using a shell command. `afplay` works nicely
  ;; for this on a Mac. To do that, you can use `start-process' like this:
  (setq party-mode-start-music-fn
    (lambda ()
      (start-process \"party-music\" \"*party-music*\" \"afplay\" \"/Users/parker/Desktop/song.mp3\")))")

(defcustom party-mode-stop-music-fn nil
  "Variable containing a function to stop music playback when party-mode is turned off
via `stop-partying`. If value is `nil`, no action will be taken to stop music playback.

You will need to customize this in a manner corresponding to how you customized
`party-mode-start-music-fn`. Below are some examples:
  ;; If you've made `party-mode-start-music-fn` an EMMS command, you can use EMMS to stop playback
  (setq party-mode-stop-music-fn 'emms-pause)
  ;; Or, if `party-mode-start-music-fn` starts a named process, stop it by killing the process
  (setq party-mode-stop-music-fn
    (lambda () (kill-process \"party-music\")))")

(defun party-mode-random-face ()
  "Return the name of a random face from available faces."
  (nth (random (length (face-list))) (face-list)))

(defun party-mode-update-visible-buffers ()
  "Randomly update faces of all visible buffers"
  (dolist (buffer (window-list))
    (set-buffer (window-buffer buffer))
    (buffer-face-set (party-mode-random-face))))

(defun party-mode-loop ()
  "Party loop"
  (party-mode-update-visible-buffers)
  (async-start
   (lambda ()
     (sleep-for 0.03))
   (lambda (result)
     (if party-mode-engaged-flag
         (party-mode-loop)))))

(defun party-mode ()
  "Partaaaayyyy!"
  (interactive)
  (setq party-mode-engaged-flag 't)
  (if party-mode-start-music-fn
      (funcall party-mode-start-music-fn)
    (message "WARNING: Please set party-mode-start-music-fn for full party experience."))
  (party-mode-loop))

(defun stop-partying ()
  "Stop party mode and reset faces to defaults"
  (interactive)
  (setq party-mode-engaged-flag nil)
  (dolist (window (window-list))
    (set-buffer (window-buffer window))
    (buffer-face-set 'default))
  (if party-mode-stop-music-fn
      (funcall party-mode-stop-music-fn)))

(provide 'party-mode)
;;; party-mode.el ends here
