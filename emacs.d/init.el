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
 '(column-number-mode t)
 '(fill-column 100)
 '(custom-theme-directory "~/.emacs.d/lisp/themes")
 '(indent-tabs-mode nil)
 '(org-default-notes-file "~/notes/captures.org")
 '(org-directory "~/notes")
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (nginx-mode apache-mode all-the-icons-ivy all-the-icons-dired all-the-icons org org-bullets markdown-mode w3m load-theme-buffer-local ansible-doc yaml-mode golden-ratio sr-speedbar ivy go-guru counsel-etags ansible rtags elpy flycheck company neotree flymake-go go-autocomplete tern-auto-complete tern go-complete jedi company-ycmd flycheck-ycmd ycmd rjsx-mode jsx-mode magit dash smartparens multi-term mo-git-blame go-mode go-playground electric-case projectile)))
 '(projectile-completion-system (quote ivy))
 '(sh-basic-offset 2)
 '(shell-prompt-pattern "^[^#$%>
]*[#$%>⇒] *")
 '(tab-width 2)
 '(tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>⇒] *\\(\\[.*\\)*"))

;; Setup package archives.  Install anything that is missing.
(require 'package)
(add-to-list 'package-archives
       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(if (not
     (string= (package-install-selected-packages) "All your packages are already installed"))
    (progn (package-refresh-contents)
     (package-install-selected-packages)))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . ansible))

(load-library "mycode.el")

(defun after-init-graphical ()
  (progn
    (load-theme 'my-blue t)
    (global-hl-line-mode)))

(defun after-init-terminal ()
  (progn
    (load-theme 'manoj-dark t)))

(add-hook 'after-init-hook
		(lambda () (if (display-graphic-p)
       (after-init-graphical)
           (after-init-terminal))))
(remove-hook 'after-init-hook 'color-theme-backup-original-values)

;; Global modes
;; Do some stuff to set up smartparens.
(require 'smartparens)
(smartparens-global-mode)
(show-smartparens-global-mode)
(setq sp-escape-quotes-after-insert nil)
(setq sp-autoescape-string-quote nil)
(sp-pair "\"" nil :actions :rem)
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)
(sp-pair "\\\"" nil :actions :rem)

;; Auto-complete
(global-auto-complete-mode)

;; Monitor whitespace problems
(require 'whitespace)
(setq whitespace-style '(face trailing empty space-after-tab space-before-tab))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Some random hooks.
(linum-mode nil)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(add-hook 'octave-mode-hook
		(lambda () (progn (setq octave-comment-char ?%)
					(setq comment-start "% ")
					(setq comment-add 0))))

(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)
(add-hook 'shell-mode-hook
		(lambda () (setq comint-process-echoes t)))
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)

;; Set up python autocomplete
(require 'jedi)
(require 'python)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
		(lambda () (progn
           (jedi:setup)
           (jedi-mode))))

;; ediff setup
(add-hook 'ediff-before-setup-hook 'save-ediff-before-windows)
(add-hook 'ediff-quit-hook 'restore-ediff-before-windows)

;; Org mode stuff.
(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; load lisp/local-org.el if it exists.
(load "local-org" t)

;;;; JIRA
(setq jiralib-url "https://jira.purestorage.com/")

;; Set up JS(X) autocomplete
(add-to-list 'load-path "/path/to/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)

;; Set up go completion
(require 'go-complete)
(require 'go-mode)
(require 'go-playground)
(add-to-list 'exec-path (getenv "GOPATH"))
(add-hook 'completion-at-point-functions 'go-complete-at-point)
(add-hook 'go-mode-hook
		(lambda () (progn
           (auto-complete-mode))))
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'before-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Set up C++ stuff
(add-hook 'c-mode-hook
		(lambda () (progn
           (company-mode)
           (flycheck-mode))))
(add-hook 'c++-mode-hook
		(lambda () (progn
           (company-mode)
           (flycheck-mode))))

;; Initialize ivy mode
(require 'ivy)
(ivy-mode)

;; ansible
(require 'ansible)
(require 'ansible-doc)

;; projectile
(require 'projectile)
(projectile-mode)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(load-library "keybindings.el")
