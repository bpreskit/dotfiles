(defun copy-line-to-end (&optional arg)
  "Copies from point to the end of the current line"
  (interactive "P")
  (kill-ring-save (point) (line-end-position arg))
  (move-end-of-line arg))

(defun copy-line-to-beginning (&optional arg)
  "Copies from point to the beginning of the current line"
  (interactive "P")
  (setq arg (if arg arg 1))
  (kill-ring-save (point) (line-beginning-position (- 2 arg)))
  (move-beginning-of-line (- 2 arg)))

(defun my-previous-window ()
  "Moves to the window to your left."
  (interactive)
  (other-window -1))

(defun my-select-first-window ()
  "Moves to the leftmost window in the frame"
  (interactive)
  (select-window (frame-first-window)))

(defun my-select-last-window ()
  "Moves to the rightmost window in the frame"
  (interactive)
  (select-window (previous-window (frame-first-window))))

(defun replace-string-in-line (&optional beg end)
  "Replaces a string, but only from point to the end of the line"
  (interactive "P")
  (setq from-str (read-string "Replace: "))
  (setq to-str (read-string (message "Replace %s with: " from-str)))
  (if beg nil (setq beg (point)))
  (if end nil (setq end (line-end-position 1)))
  (replace-string from-str to-str nil beg end))

(defun beginning-of-line-dwim ()
  "In wrapped lines, goes to beginning of this wrap, then to the actual beginning of the line.  Cooperates with indentation if you're at the beginning and there's whitespace."
  (interactive)
  (if (= (current-column) 0)
      (back-to-indentation)
    (let ((start-pos (point)))
      (beginning-of-visual-line)
      (if (= start-pos (point))
	  (move-beginning-of-line nil)
	nil))))

(defun end-of-line-dwim ()
  "In wrapped lines, goes to end of this wrap, then to the actual end of the line"
  (interactive)
  (let ((start-pos (point)))
    (end-of-visual-line)
    (if (= (line-end-position) (point))
	nil
      (backward-char))
    (if (= (point) start-pos)
	(move-end-of-line nil)
      nil)))

(defun switch-buffer-previous-window ()
  "Switches the buffers displayed in this window and the previous one"
  (interactive)
  (let ((pw (previous-window))
	(pb (window-buffer (previous-window)))
	(curb (current-buffer))
	(cw (selected-window)))
    (set-window-buffer pw curb)
    (switch-to-buffer pb)
    (select-window cw)))

(defun switch-buffer-next-window ()
  "Switches the buffers displayed in this window and the next one"
  (interactive)
  (let ((nw (next-window))
	(nb (window-buffer (next-window)))
	(curb (current-buffer))
	(cw (selected-window)))
    (set-window-buffer nw curb)
    (switch-to-buffer nb)
    (select-window cw)))
