(require 'lsp-mode)

(add-hook 'rust-mode-hook 'lsp-deferred)
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(setq rust-format-on-save t)
