;;;###autoload
(defgroup doc-mode nil
  "Options for doc-mode"
  :tag "Doc Mode")

(defcustom doc-mode-body-width 100
  "The desired body width in `olivetti-mode` when in `doc-mode`.")
(defcustom doc-mode-variable-pitch t
  "If non-nil, set variable-pitch-mode in doc-mode.")

(defun doc-mode-propagate-fixed-pitch ()
  (interactive)
  (dolist
      (face '(org-block-begin-line
              org-block-end-line
              org-block
              org-code
              org-verbatim
              org-special-keyword
              org-meta-line
              org-checkbox))
      (set-face-attribute face nil :inherit 'fixed-pitch)))

;;;###autoload
(defcustom doc-mode-theme 'spacemacs-light
  "The theme to use when `doc-mode` is active.")

(defvar-local doc-mode-original-themes nil
  "Stores the list of active themes before `doc-mode` was enabled.")

(defface doc-mode-hide-face
  '((t (:background "white" :foreground "white")))
  "A face to hide text by matching background and foreground colors.")

(define-minor-mode doc-mode
  "A minor mode for a distraction-free writing environment."
  :group 'editing
  :global nil
  :lighter " MyDoc"
  (if doc-mode
      ;; --- On Activation ---
      (progn

        ;; 1. Olivetti mode with custom width
        (setq-local olivetti-body-width doc-mode-body-width)
        (olivetti-mode 1)

        ;; 2. Hide leading stars in Org-mode
        (setq-local doc-mode-original-leading-bullet org-superstar-leading-bullet)
        (setq-local org-superstar-leading-bullet " ")   ;; Make the headings less spaced out
        (org-superstar-restart)

        ;; 3. Set theme
        (setq-local doc-mode-original-themes custom-enabled-themes)
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme doc-mode-theme t)
        ;; 4. Variable pitch mode
        (when doc-mode-variable-pitch
            (variable-pitch-mode 1)
          ;; 4a. Don't want parentheses to be bolded in variable
            (setq-local doc-mode-spm-weight (face-attribute 'show-paren-match :weight))
            (set-face-attribute 'show-paren-match nil :weight 'unspecified))
        ;; Unset auto-fill-mode
        (setq-local prev-auto-fill-mode (not (eq auto-fill-function nil)))
        (auto-fill-mode -1)

        (doc-mode-propagate-fixed-pitch))
    ;; --- On Deactivation ---
    (progn
      ;; 1. Revert variable-pitch-mode
      (when doc-mode-variable-pitch
        (variable-pitch-mode -1)
        (set-face-attribute 'show-paren-match nil :weight doc-mode-spm-weight))

      ;; 2. Revert olivetti-mode
      (olivetti-mode -1)

      ;; 3. Show leading stars
      (setq org-superstar-leading-bullet doc-mode-original-leading-bullet)
      (org-superstar-restart)

      ;; Reset auto-fill-mode
      (auto-fill-mode prev-auto-fill-mode)

      ;; 4. Restore original themes
      (disable-theme doc-mode-theme)
      (mapc #'load-theme doc-mode-original-themes))))
