;; Bunch of variables.

;; This is where the custom variables are
(setq custom-file (file-truename "~/.emacs.d/lisp/global-variables.el"))

;; Customized variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auth-source-save-behavior nil)
 '(byte-compile-warnings nil)
 '(column-number-mode t)
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(help-window-select t)
 '(inhibit-startup-screen t)
 '(lsp-keymap-prefix (kbd "C-x n"))
 '(org-export-with-sub-superscripts '{})
 '(org-use-sub-superscripts '{})
 '(package-selected-packages
   '(doom-modeline ligature org-modern org-superstar olivetti lua-mode haskell-mode uml-mode flycheck-plantuml term-keys "termkeys" magit magit-delta lsp-ivy lsp-ui elsa gruvbox-theme orderless inhibit-mouse rg which-key ivy-hydra marginalia protobuf-mode zenburn-theme clipetty xclip ox-pandoc pandoc counsel project xref xref-union plantuml-mode python-black gnu-elpa-keyring-update lsp-pyright blacken rust-mode cmake-mode lsp-mode systemd json-mode groovy-mode tide nginx-mode apache-mode all-the-icons all-the-icons-completion all-the-icons-dired all-the-icons-ivy org org-bullets markdown-mode ansible-doc yaml-mode ivy ansible elpy flycheck company company-ycmd flycheck-ycmd ycmd dash smartparens multi-term go-mode electric-case projectile swiper))
 '(safe-local-variable-values
   '((projectile-indexing-method . native)
     (projectile-enable-caching . t)
     (blacken-mode . t)
     (org-agenda-skip-archived-trees . t)
     (org-sparse-tree-open-archived-trees . t)
     (org-cycle-open-archived-trees . t)))
 '(shell-prompt-pattern "^[^#$%>\12]*[#$%>⇒] *")
 '(tab-width 4)
 '(tramp-shell-prompt-pattern "\\(?:^\\|\15\\)[^]#$%>\12]*#?[]#$%>⇒] *\\(\33\\[.*\\)*"))
;; Customized faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 128 :family "DejaVu Sans Mono"))))
 '(italic ((t (:slant italic))))
 '(magit-hash ((t (:foreground "yellow")))))
