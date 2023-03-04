;; Set up C++ stuff
(require 'cc-mode)
(require 'lsp-mode)
(require 'company)
(require 'flycheck)

(add-hook 'c-mode-hook
		(lambda () (progn
           (company-mode)
           (flycheck-mode))))
(add-hook 'c++-mode-hook
		(lambda () (progn
           (company-mode)
           (flycheck-mode))))

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
