;; Basic setup for lsp-mode
(defun my/lsp-describe ()
  "`lsp-describe-thing-at-point' but it jumps to the help buffer"
  (interactive)
  (lsp-describe-thing-at-point)
  (switch-to-buffer-other-window "*lsp-help*"))
(use-package lsp-mode
  :custom
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  (lsp-idle-delay 0.1)
  (eldoc-idle-delay 0)
  :bind
  (:map
   lsp-mode-map
   ("C-c C-d" . my/lsp-describe)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-references] . lsp-find-references)
   :map
   lsp-signature-mode-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("M-<right>" . lsp-signature-next)
   ("M-<left>" . lsp-signature-previous)
   :map
   lsp-command-map
   ("a r" . lsp-rename)))
(use-package lsp-ivy
  :ensure t
  :bind
  (:map
   lsp-mode-map
      ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)))

;; lsp-ui setup
(defun my/lsp-ui-doc-unfocus-and-describe ()
  "Unfocus from lsp-ui-doc frame and describe"
  (interactive)
  (lsp-ui-doc-unfocus-frame)
  (lsp-describe-thing-at-point)
  (switch-to-buffer-other-window "*lsp-help*"))
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-eldoc-enable-hover nil)
  :bind
  (:map
   lsp-command-map
   ("h o" . lsp-ui-doc-focus-frame)
   :map
   lsp-ui-doc-frame-mode-map
   ("d" . my/lsp-ui-doc-unfocus-and-describe)))
(add-hook 'lsp-mode-hook
          'yas-minor-mode-on)

;; Set up language-specific stuff.
(dolist (lib
     (list
      "cpp-conf"
      "golang-conf"
      "java-conf"
      "python-conf"
      "rust-conf"
      ;; "augment-conf"
      ))
  (load-library lib))

(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]site-packages\\'")
