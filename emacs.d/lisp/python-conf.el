(require 'python)
(require 'lsp-mode)
(require 'lsp-jedi)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () lsp-jedi-executable-command))
  :major-modes '(python-mode cython-mode)
  :priority 100
  :server-id 'jedi
  :library-folders-fn (lambda (_workspace) lsp-jedi-python-library-directories)
  :initialization-options (lambda () (gethash "jedi" (lsp-configuration-section "jedi")))))

(add-hook 'python-mode-hook 'lsp-deferred)
