(require 'lsp-mode)

;; Use rust-analyzer, not rls.
(remhash 'rls lsp-clients)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(setq rust-format-on-save t)
