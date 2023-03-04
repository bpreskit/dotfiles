(require 'lsp-mode)
(require 'cc-mode)

(add-hook 'java-mode-hook 'lsp-deferred)
