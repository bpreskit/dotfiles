(require 'lsp-mode)

;; Use rust-analyzer, not rls.
(remhash 'rls lsp-clients)
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(setq rust-format-on-save t)
