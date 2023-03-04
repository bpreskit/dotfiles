;; Org mode stuff.
(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; load lisp/custom-org.el, and local-org.el if it exists.
(load-library "custom-org")
(load "local-org" t)
