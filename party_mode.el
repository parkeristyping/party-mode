(setq party-mode-engaged nil)

(defun random-face ()
  "Return the name of a random face from available faces."
  (nth (random (length (face-list))) (face-list)))

(defun random-visible-buffer ()
  "Return a random visible buffer."
  (window-buffer (nth (random (length (window-list))) (window-list))))

(defun party-mode-step ()
  "Randomly update a visible buffer's face"
  (set-buffer (random-visible-buffer))
  (buffer-face-set (random-face)))

(defun party-loop ()
  "Party loop"
  (party-mode-step)
  (async-start
   (lambda ()
     (sleep-for 0.1))
   (lambda (result)
     (if party-mode-engaged
         (party-loop)))))

(defun party-mode ()
  "Partaaaayyyy!"
  (interactive)
  (setq party-mode-engaged 't)
  (emms-play-file (concat (file-name-directory #$) "party_music.mp3"))
  (party-loop))

(defun work-mode ()
  "Stop party mode and reset faces to defaults"
  (interactive)
  (setq party-mode-engaged nil)
  (dolist (window (window-list))
    (set-buffer (window-buffer window))
    (buffer-face-set 'default))
  (emms-stop))
