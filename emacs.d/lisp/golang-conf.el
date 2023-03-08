;; Set up go completion
(require 'go-mode)
(require 'lsp-mode)
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(add-to-list 'exec-path (concat (getenv "GOROOT") "/bin"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook 'lsp-deferred)
