;; Org mode stuff.
(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (progn
                                      (auto-fill-mode t)
                                      (org-bullets-mode 1)
                                      (company-mode -1))))

;; load lisp/custom-org.el, and local-org.el if it exists.
(load-library "custom-org")
(load "local-org" t)
