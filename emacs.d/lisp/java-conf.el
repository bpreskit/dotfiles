(require 'lsp-mode)
(require 'java-mode)

(add-hook 'java-mode-hook 'lsp-deferred)
