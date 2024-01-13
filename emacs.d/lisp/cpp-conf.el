;; Set up C++ stuff
(require 'cc-mode)
(require 'lsp-mode)
(require 'flycheck)

(add-hook 'c-mode-hook
		(lambda () (progn
           (flycheck-mode))))
(add-hook 'c++-mode-hook
		(lambda () (progn
           (flycheck-mode))))

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
