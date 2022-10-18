;; Set up go completion
(require 'go-mode)
(require 'go-playground)
(require 'lsp-mode)
(add-to-list 'exec-path (getenv "GOPATH"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook 'lsp-deferred)
