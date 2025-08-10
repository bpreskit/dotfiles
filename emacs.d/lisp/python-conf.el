(use-package python
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/venvs/my-venv/bin")))
(use-package lsp-pyright)
(use-package blacken
  :custom
  (blacken-line-length 'fill))
