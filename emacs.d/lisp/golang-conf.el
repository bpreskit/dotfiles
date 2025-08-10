;; Set up go completion
(use-package go-mode
  :config
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
  (add-to-list 'exec-path (concat (getenv "GOROOT") "/bin")))
