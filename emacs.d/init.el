;; Added by Package.el.
(package-initialize)

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
