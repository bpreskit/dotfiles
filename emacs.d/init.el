;; Added by Package.el.  (package-initialize)

(setq inhibit-startup-screen t)
(setq column-number-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Bitstream Vera Sans Mono" :foundry "Bits" :slant normal :weight normal :height 105 :width normal))))
 '(magit-hash ((t (:foreground "yellow"))))
 '(italic ((t (:slant italic)))))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (golden-ratio sr-speedbar ivy go-guru counsel-etags ansible rtags elpy flycheck company neotree flymake-go go-autocomplete tern-auto-complete tern go-complete jedi company-ycmd flycheck-ycmd ycmd rjsx-mode jsx-mode magit dash smartparens multi-term mo-git-blame go-mode electric-case))))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
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

(load-library "mycode.el")
(require 'color-theme)
(message "Loading color-themes from init.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (message "tryna set the theme...")))
(add-hook 'after-init-hook
	  (lambda () (color-theme-deep-blue)))
(remove-hook 'after-init-hook 'color-theme-backup-original-values)

;; Do some stuff to set up smartparens.
(require 'smartparens)
(smartparens-global-mode)
(show-smartparens-global-mode)
(setq sp-escape-quotes-after-insert nil)
(setq sp-autoescape-string-quote nil)
(sp-pair "\"" nil :actions :rem)
(sp-pair "'" nil :actions :rem)
(sp-pair "\\\"" nil :actions :rem)

;; Monitor whitespace problems
(require 'whitespace)
(setq whitespace-style '(face trailing empty space-after-tab space-before-tab))

;; Some random hooks.
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(add-hook 'octave-mode-hook
	  (lambda () (progn (setq octave-comment-char ?%)
			    (setq comment-start "% ")
			    (setq comment-add 0))))
(add-hook 'inferior-octave-mode-hook
	  (lambda ()
	    (progn
	      (define-key inferior-octave-mode-map (kbd "C-M-n") 'forward-sexp)
	      (define-key inferior-octave-mode-map
		(kbd "C-M-p") 'backward-sexp))))

(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)
(add-hook 'shell-mode-hook
	  (lambda () (setq comint-process-echoes t)))


;; Set up python autocomplete
(require 'jedi)
(define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
(define-key jedi-mode-map (kbd "C-x 4 M-.") (lambda () (interactive) (jedi:goto-definition t)))
(define-key jedi-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
(define-key jedi-mode-map (kbd "C-c .") nil)
(define-key jedi-mode-map (kbd "C-c ,") nil)
(define-key jedi-mode-map (kbd "C-c ?") nil)
(define-key jedi-mode-map (kbd "C-c C-d") 'jedi:show-doc)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
	  (lambda () (progn
		       (jedi:setup)
		       (jedi-mode)
		       (define-key python-mode-map (kbd "C-c u") 'python-nav-backward-up-list)
		       (define-key python-mode-map (kbd "C-c d") 'python-nav-up-list)
		       (define-key python-mode-map (kbd "C-M-i") 'jedi:complete))))

;; Org mode stuff.
(require 'org)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
(define-key org-mode-map (kbd "M-j") 'org-meta-return)
(define-key org-mode-map (kbd "M-J") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-M-u") 'org-up-element)
(define-key org-mode-map (kbd "C-M-d") 'org-down-element)
(define-key org-mode-map (kbd "M-d") 'org-table-kill-row)
(define-key org-mode-map (kbd "M-n") 'org-next-item)
(define-key org-mode-map (kbd "M-p") 'org-previous-item)

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
(add-to-list 'exec-path (getenv "GOPATH"))
(add-hook 'completion-at-point-functions 'go-complete-at-point)
(add-hook 'go-mode-hook
	  (lambda () (progn
		       (auto-complete-mode))))
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "C-x 4 M-.") '(lambda () (interactive) (godef-jump 'point t)))
(define-key go-mode-map (kbd "C-h d") 'godoc)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

;; Set up C++ stuff
(add-hook 'c-mode-hook
	  (lambda () (progn
		       (company-mode)
		       (flycheck-mode))))
(add-hook 'c++-mode-hook
	  (lambda () (progn
		       (company-mode)
		       (flycheck-mode))))

(require 'ivy)
(ivy-mode)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Set my key shortcuts.

(global-set-key (kbd "C-x ;") 'copy-line-to-end)
(global-set-key (kbd "C-x :") 'copy-line-to-beginning)
(global-set-key (kbd "C-x p") 'my-previous-window)
(global-set-key (kbd "C-x <") 'my-select-first-window)
(global-set-key (kbd "C-x >") 'my-select-last-window)
(global-set-key (kbd "C-x 7") 'delete-other-windows-vertically)
(global-set-key (kbd "C-c t") 'revert-buffer)
(global-set-key (kbd "C-c ;") 'replace-string-in-line)
(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
(global-set-key (kbd "C-e") 'end-of-line-dwim)
(global-set-key (kbd "C-c p") 'switch-buffer-previous-window)
(global-set-key (kbd "C-c o") 'switch-buffer-next-window)
(global-set-key (kbd "C-M-u") 'sp-backward-up-sexp)
(global-set-key (kbd "C-M-d") 'sp-down-sexp)
(global-set-key (kbd "C-M-e") 'sp-up-sexp)
(global-set-key (kbd "C-M-a") 'sp-backward-down-sexp)
(global-set-key (kbd "C-M-f") 'sp-forward-sexp)
(global-set-key (kbd "C-M-b") 'sp-backward-sexp)
(global-set-key (kbd "C-M-n") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-x 4 .") (kbd "C-x 4 M-."))
(global-set-key (kbd "C-x 4 M-.") 'xref-find-definitions-other-window)
(global-set-key (kbd "C-c 2") 'golden-window-split-down)
(global-set-key (kbd "C-c 3") 'golden-window-split-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c +") 'golden-cycle)
