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
(global-set-key (kbd "C-M-i") 'auto-complete)
(global-set-key (kbd "C-x 1") 'delete-other-windows-or-restore)
(global-set-key (kbd "C-c 1") 'delete-other-windows)
(global-set-key (kbd "M-g r") 'vc-git-grep)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-<delete>") 'delete-region)
(global-set-key (kbd "M-n") 'next-defun)
(global-set-key (kbd "M-p") 'prev-defun)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-x .") 'projectile-command-map)

;; (require 'jedi)
;; (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
;; (define-key jedi-mode-map (kbd "C-x 4 M-.") (lambda () (interactive) (jedi:goto-definition t)))
;; (define-key jedi-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
;; (define-key jedi-mode-map (kbd "C-c .") nil)
;; (define-key jedi-mode-map (kbd "C-c ,") nil)
;; (define-key jedi-mode-map (kbd "C-c ?") nil)
;; (define-key jedi-mode-map (kbd "C-c C-d") 'jedi:show-doc)
;; (define-key jedi-mode-map (kbd "C-M-i") 'jedi:complete)

(require 'python)
(define-key python-mode-map (kbd "C-c u") 'python-nav-backward-up-list)
(define-key python-mode-map (kbd "C-c d") 'python-nav-up-list)
(define-key python-mode-map (kbd "M-n") 'python-nav-forward-defun)
(define-key python-mode-map (kbd "M-p") 'python-nav-backward-defun)

(require 'org)
(define-key org-mode-map (kbd "M-j") 'org-meta-return)
(define-key org-mode-map (kbd "C-c RET") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "M-J") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "C-M-u") 'org-up-element)
(define-key org-mode-map (kbd "C-M-d") 'org-down-element)
(define-key org-mode-map (kbd "M-n") 'org-next-item)
(define-key org-mode-map (kbd "M-p") 'org-previous-item)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(require 'go-mode)
(require 'go-playground)
(define-key go-mode-map (kbd "C-h d") 'godoc)
(define-key go-playground-mode-map (kbd "C-c RET") 'go-playground-exec)

(require 'ansible)
(require 'ansible-doc)
(define-key ansible-key-map (kbd "C-h d") 'ansible-doc)
(define-key ansible-doc-module-mode-map (kbd "m") 'ansible-doc)
(define-key ansible-doc-module-mode-map (kbd "d") 'ansible-doc)

(require 'octave)
(define-key inferior-octave-mode-map (kbd "C-M-n") 'forward-sexp)
(define-key inferior-octave-mode-map (kbd "C-M-p") 'backward-sexp)

(require 'w3m)
(define-key w3m-mode-map (kbd "C-c >") 'w3m-tab-next-buffer)
(define-key w3m-mode-map (kbd "C-c <") 'w3m-tab-previous-buffer)

(require 'man)
(define-key Man-mode-map (kbd "l") 'recenter-top-bottom)

(require 'magit)
(define-key magit-blob-mode-map (kbd "RET") 'magit-blob-visit-file)
