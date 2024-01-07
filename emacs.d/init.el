;; Added by Package.el.
(package-initialize)

(setq inhibit-startup-screen t)
(setq column-number-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 128 :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(italic ((t (:slant italic))))
 '(magit-hash ((t (:foreground "yellow")))))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))


;; Load setup code code!
(dolist (lib
     (list
      "global-variables"
      "install-packages"
      "mycode"
      "setup-misc"
      "setup-lsp"
      "setup-org"
      "keybindings"))
  (load-library lib))
(load "local-init.el" t)
