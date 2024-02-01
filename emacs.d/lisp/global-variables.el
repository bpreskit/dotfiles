;; Bunch of variables.
(setq custom-file "~/.emacs/global-variables.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-keymap-prefix "n"))

(setq
 ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
 column-number-mode t
 custom-theme-directory "~/.emacs.d/lisp/themes"
 fill-column 100
 indent-tabs-mode nil
 package-selected-packages
   '(lsp-pyright blacken rust-mode cmake-mode lsp-mode systemd json-mode groovy-mode tide nginx-mode apache-mode all-the-icons-ivy all-the-icons-dired all-the-icons org org-bullets markdown-mode load-theme-buffer-local ansible-doc yaml-mode ivy ansible elpy flycheck company flymake-go go-autocomplete go-complete company-ycmd flycheck-ycmd ycmd magit dash smartparens multi-term mo-git-blame go-mode electric-case projectile swiper)
 projectile-completion-system 'ivy
 sh-basic-offset 2
 shell-prompt-pattern "^[^#$%>
]*[#$%>⇒] *"
 tab-width 2
 tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>⇒] *\\(\\[.*\\)*"
 ediff-window-setup-function 'ediff-setup-windows-plain
 calendar-date-style "iso"
)
