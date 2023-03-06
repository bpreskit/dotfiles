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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(fill-column 100)
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(indent-tabs-mode nil)
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "xreader"
      (file))
     ("\\.mp3\\'" "rhythmbox"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "xviewer"
      (file))
     ("\\.ods\\'" "libreoffice"
      (file))
     ("\\.odt\\'" "libreoffice"
      (file)))))
 '(package-selected-packages
   (quote
    (blacken rust-mode cmake-mode lsp-jedi lsp-mode systemd json-mode groovy-mode tide openwith ox-slimhtml nginx-mode apache-mode all-the-icons-ivy all-the-icons-dired all-the-icons org org-bullets markdown-mode w3m load-theme-buffer-local ansible-doc yaml-mode golden-ratio ivy counsel-etags ansible elpy flycheck company neotree flymake-go go-autocomplete go-complete jedi company-ycmd flycheck-ycmd ycmd magit dash smartparens multi-term mo-git-blame go-mode go-playground electric-case projectile)))
 '(projectile-completion-system (quote ivy))
 '(sh-basic-offset 2)
 '(shell-prompt-pattern "^[^#$%>
]*[#$%>⇒] *")
 '(tab-width 2)
 '(tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>⇒] *\\(\\[.*\\)*"))

;; Load setup code code!
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
