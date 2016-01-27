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
