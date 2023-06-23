;; -*- mode: Lisp; lexical-binding: t; -*-
(require 'find-file)

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

(defun golden-window-split-down (&optional rev)
  (interactive "P")
  (let ((size (round (* (if rev -0.618 0.618) (window-height)))))
    (split-window-below size)))

(defun golden-window-split-right (&optional rev)
  (interactive "P")
  (let ((size (round(* (if rev 0.382 0.618) (window-width)))))
    (split-window-right size)))

(defun my-git-blame ()
  (interactive)
  (golden-window-split-right t)
  (mo-git-blame-current))

;; (defun my-git-blame-quit ()
;;   (interactive)
;;   )

(defun golden-cycle (&optional horiz)
  "Rotates the height of the current window through (phi, 1 - phi) as
  a percentage of the frame height"
  (interactive "P")
  (let*
      ((frame-dim (if horiz
                      (frame-width)
                    (frame-height)))
       (window-dim (if horiz
                       (window-width)
                     (window-height)))
       (gold-ratio 0.618)
       (gold-ratios (list gold-ratio (- 1 gold-ratio)))
       (gold-heights
	(mapcar
   (lambda (rt) (floor (* rt frame-dim)))
   gold-ratios))
       (dists
	(mapcar
   (lambda (ht) (abs (- ht window-dim)))
   gold-heights))
       (minimum (apply 'min dists))
       (argmin (position minimum dists))
       (ratio
	(if (= minimum 0)
      (nth (mod (+ 1 argmin) (length gold-ratios)) gold-ratios)
    (nth argmin gold-ratios))))
	(resize-to-fraction ratio horiz)))

(defun set-window-dim (dim &optional horiz)
  (interactive "P")
  (if horiz
      (progn
	(setq dim-fun 'enlarge-window-horizontally)
	(setq cur-dim (window-width)))
    (progn
      (setq dim-fun 'enlarge-window)
      (setq cur-dim (window-height))))
  (funcall dim-fun (- dim cur-dim)))

(defun resize-to-fraction (&optional frac horiz)
  (interactive "P")
  (if (null frac)
      (setq frac 0.5))
  (multiple-value-bind (dim cur-dim dim-fun)
      (if horiz
    (list (frame-width)
                (window-width)
                'enlarge-window-horizontally)
	(list (frame-height)
              (window-height)
              'enlarge-window))
    (let ((target-dim (floor (* frac dim))))
      (funcall dim-fun (- target-dim cur-dim)))))

(let (win-conf linum-mode-state)
  (defun delete-other-windows-or-restore ()
    "Basically does (delete-other-windows), but saves your window config.  If you run it while there's only one window open, it restores from the saved config."
    (interactive)
    (let ((one-window (= 1 (count-windows)))
    (saved-conf (not (null win-conf))))
      (catch 'done
	(if (and one-window saved-conf)
      (progn
              (linum-mode linum-mode-state)
        (set-window-configuration win-conf)
        (setq win-conf nil)
        (throw 'done nil)))
	(if (not one-window)
      (progn
        (setq win-conf (current-window-configuration))
              (setq linum-mode-state (if linum-mode t -1))
        (delete-other-windows)
              (linum-mode -1)
        (throw 'done nil)))))))

(defvar ediff-before-windows)

(defun save-ediff-before-windows ()
  (setq ediff-before-windows (current-window-configuration)))

(defun restore-ediff-before-windows ()
  (set-window-configuration ediff-before-windows))

(defun pwd (&optional kill-pwd)
  "Print current directory.  With optional prefix argument, yank to kill ring as well."
  (interactive "P")
  (message "%s" default-directory)
  (if kill-pwd
      (kill-new default-directory)))

(defun ansi-python ()
  "Runs ansi-term but opens up a python3 shell instead."
  (interactive)
  (ansi-term "/usr/bin/python3" "ansi-python"))

(defun toggle-dedicated-window ()
  "Toggle whether this window is both dedicated to its buffer.  Uses `set-window-dedicated-p'."
  (interactive)
  (let ((window (selected-window))
        (buffer (current-buffer)))
    (let ((protected (window-dedicated-p window)))
    (set-window-dedicated-p window (not protected)))))

(defun linum-mode-ifnt-log ()
  (if (not (and
           (stringp (buffer-file-name))
           (string-match ".*\\(\.\\|\\(sys\\)\\)log.*\\(\.tar\\)?\\(\.gz\\)?" (buffer-file-name))))
      (linum-mode 1)))

(defun copy-file-name (&optional basename-p)
  "Copy name of file pointed to by this buffer as kill.
With optional prefix argument, copy only the basename."
  (interactive "P")

  (if (buffer-file-name)
      (let ((file-name
             (if basename-p
                 (ff-basename (buffer-file-name))
               (buffer-file-name))))
        (progn (kill-new file-name)
               (message file-name)))
    (message "%s is not in a file" (buffer-name))))

(defun prev-defun (&optional arg)
  (interactive "p")
  (let ((defun-count (if arg arg 1))
        (start-pos (point)))
    (progn
      (beginning-of-defun defun-count)
      (if (= (point) start-pos)
          (forward-line (* -1 arg))))))

(defun next-defun (&optional arg)
  (interactive "p")
  (prev-defun (* -1 arg)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun open-as-sudo (&optional remove-buffer-p)
  "Open the file pointed to by this buffer as sudo.
With optional prefix argument, also kill this buffer."

  (interactive "P")
  (let ((this-file (buffer-file-name)))
    (if
     (and
      this-file
      (not (tramp-tramp-file-p this-file)))
     (progn
       (if remove-buffer-p (kill-buffer))
       (find-file (concat "/sudo::" this-file))))))
