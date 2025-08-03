;; Added by Package.el.
(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))


;; Load setup code code!
;; Initialize variables
(load-library "global-variables")
(load-library "custom-variables")
(load "local-custom-vars.el" t)
;; Load remaining code
(dolist (lib
     (list
      "install-packages"
      "mycode"
      "setup-misc"
      "setup-lsp"
      "setup-org"
      "keybindings"))
  (load-library lib))
(load "local-init.el" t)
