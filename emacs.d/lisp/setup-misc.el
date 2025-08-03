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
(use-package smartparens
  :custom
  (sp-escape-quotes-after-insert nil)
  (sp-autoescape-string-quote nil)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-pair "\"" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "\\\"" nil :actions :rem)
  )

;; Auto-complete
(use-package company
  :init
  (global-company-mode)
)

;; Monitor whitespace problems
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing empty space-after-tab space-before-tab))
)
(setq-default indent-tabs-mode nil)

;; Some random hooks.
(display-line-numbers-mode nil)
(add-hook 'find-file-hook 'linum-mode-ifnt-log)
(use-package all-the-icons
  :if my/use-all-the-icons
  :config
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts)))
(use-package all-the-icons-dired
  :if (and my/use-all-the-icons my/use-all-the-icons-dired)
  :hook dired-mode
)
(use-package all-the-icons-ivy
  :if (and my/use-all-the-icons my/use-all-the-icons-ivy)
  :after (ivy)
  :custom (all-the-icons-spacer . ("  "))
  :init (all-the-icons-ivy-setup))
(use-package marginalia
  :custom (marginalia-align 'left)
  :init (marginalia-mode)
)

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
(use-package ivy
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
)

;; ansible
(require 'ansible)
(require 'ansible-doc)

;; projectile
(use-package projectile
  :config
  (projectile-mode)
  )

;; Turn off scroll bars, menu bars, tool bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(use-package inhibit-mouse
  :ensure t
  :custom
  (inhibit-mouse-button-numbers '(2 3 4 5))
  :config
  (inhibit-mouse-mode 1))

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
