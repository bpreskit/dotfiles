(require 'python)
(require 'lsp-mode)
(require 'lsp-pyright)
(require 'blacken)

(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
(setq blacken-line-length 'fill)

(add-hook 'python-mode-hook 'lsp-deferred)
