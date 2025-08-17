;; -*- lexical-binding: t -*-
;; Initialize ivy mode
(use-package orderless
  :ensure t
  :custom
  ;; You can add other styles here as fallbacks.
  ;; `basic` is often a good idea for file and TRAMP completion.
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

(defun my/make-ivy-action-doer (action exiting-call-p)
  (lambda ()
    (interactive)
    (ivy-set-action action)
    (if exiting-call-p (ivy-done) (ivy-call))))
(setq my/factorized-do-ivy-copy (my/make-ivy-action-doer #'ivy--action-copy nil))
(setq my/factorized-do-ivy-copy-and-exit (my/make-ivy-action-doer #'ivy--action-copy t))

(use-package ivy
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus) (t . orderless-ivy-re-builder)))
  :config
  (ivy-mode)
  (add-to-list
   'ivy-highlight-functions-alist
   '(orderless-ivy-re-builder . orderless-ivy-highlight))
  :bind
  (:map
   ivy-minibuffer-map
   ("C-RET" . 'ivy-call)
   ("C-M-o" . 'ivy-dispatching-done)
   ("M-o" . 'ivy-dispatching-call)))

(define-key ivy-minibuffer-map [remap kill-ring-save] (my/make-ivy-action-doer #'ivy--action-copy nil))

;; Make ivy prefer exact matches, even when using orderless
(defun my/ivy-recompute-index-prefer-match (_re-str cands)
  "Recompute index of selected candidate.
This function serves as a fallback when nothing else is available."
  (or (cl-position ivy-text cands :test #'string=) 0))
(setf (alist-get t ivy-index-functions-alist) 'my/ivy-recompute-index-prefer-match)

(defun my/company-common-or-ivy ()
    (interactive)
    (if (company--active-p) (counsel-company) (company-complete-common)))
