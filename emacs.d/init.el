(setq inhibit-startup-screen 1)
(setq column-number-mode 1)
(auto-revert-mode 1)
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(add-hook 'find-file-hook (lambda () (set-face-attribute 'default nil :height 105)))
;; (add-hook 'octave-mode-hook
;; 	  (progn (lambda () (setq octave-comment-char ?%))
;; 		 (lambda () (setq comment-start "% "))
;; 		 (lambda () (setq comment-add 0))
;; 	 ;; (define-key octave-mode-map (kbd "C-M-n") 'forward-sexp)
;; 	 ;; (define-key octave-mode-map (kbd "C-M-p") 'backward-sexp)
;; 		 ))
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
	
	 
(setq python-shell-interpreter "python3")
(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)

(require 'smartparens)
(smartparens-global-mode)
(sp-pair "\"" nil :actions :rem)
(sp-pair "'" nil :actions :rem)
(setq sp-escape-quotes-after-insert nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Bitstream Vera Sans Mono" :foundry "Bits" :slant normal :weight normal :height 105 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(load-library "mycode.el")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))

(if (display-graphic-p)
    (color-theme-deep-blue)
  (color-theme-tty-dark))

(require 'package) ;; You might already have this line
(require 'helm)
(require 'git)

(global-set-key (kbd "C-;") 'copy-line-to-end)
(global-set-key (kbd "C-:") 'copy-line-to-beginning)
(global-set-key (kbd "C-x p") 'my-previous-window)
(global-set-key (kbd "C-x <") 'my-select-first-window)
(global-set-key (kbd "C-x >") 'my-select-last-window)
(global-set-key (kbd "C-M-;") 'replace-string-in-line)
(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
(global-set-key (kbd "C-e") 'end-of-line-dwim)
(global-set-key (kbd "C-c p") 'switch-buffer-previous-window)
(global-set-key (kbd "C-c o") 'switch-buffer-next-window)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
