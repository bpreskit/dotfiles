(require 'lsp-mode)
(add-hook 'lsp-mode-hook
          'yas-minor-mode-on)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1
      eldoc-idle-delay 0
)

;; Set up language-specific stuff.
(dolist (lib
     (list
      "cpp-conf"
      "golang-conf"
      "java-conf"
      "python-conf"
      "rust-conf"
      ))
  (load-library lib))

(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]site-packages\\'")
