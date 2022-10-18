(require 'python)
(require 'lsp-mode)
(require 'lsp-jedi)

(add-hook 'python-mode-hook 'lsp-deferred)
