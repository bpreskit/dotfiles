;; Make modes happen.
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . ansible))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Use json-mode on json files.
(setq auto-mode-alist (remove '("\\.json\\'" . javascript-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(setq js-indent-level 2)

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
(require 'company)
(global-company-mode)

;; Monitor whitespace problems
(require 'whitespace)
(setq whitespace-style '(face trailing empty space-after-tab space-before-tab))
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq-default indent-tabs-mode nil)

;; Some random hooks.
(display-line-numbers-mode nil)
(add-hook 'find-file-hook 'linum-mode-ifnt-log)
(require 'all-the-icons)
(require 'all-the-icons-dired)
(if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts))
(add-hook 'dired-mode-hook '(lambda () (if (display-graphic-p) (all-the-icons-dired-mode))))

;; Octave stuff.
(add-hook 'octave-mode-hook
		(lambda () (progn (setq octave-comment-char ?%)
					(setq comment-start "% ")
					(setq comment-add 0))))

;; Not sure what hide-ifdef is for.
(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)

;; ediff setup
(add-hook 'ediff-before-setup-hook 'save-ediff-before-windows)
(add-hook 'ediff-quit-hook 'restore-ediff-before-windows)

;; Initialize ivy mode
(require 'ivy)
(ivy-mode)
(setq ivy-use-selectable-prompt t)

;; ansible
(require 'ansible)
(require 'ansible-doc)

;; projectile
(require 'projectile)
(projectile-mode)

;; Turn off scroll bars, menu bars, tool bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

(add-hook 'before-save-hook 'executable-make-buffer-file-executable-if-script-p)
