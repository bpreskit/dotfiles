(require 'lsp-mode)
(add-hook 'lsp-mode-hook
          'yas-minor-mode-on)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1)  ;; clangd is fast
