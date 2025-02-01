(require 'python)
(require 'lsp-mode)
(require 'lsp-pyright)
(require 'blacken)

(add-to-list 'exec-path (concat "~/.local/bin"))
(add-to-list 'exec-path (concat "~/.local/venvs/my_venv/bin"))
(setq blacken-line-length 'fill)

(add-hook 'python-mode-hook 'lsp-deferred)
