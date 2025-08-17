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

;; Bring in ivy
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

;; Create new actions
(defun my/make-ivy-action-doer (action exiting-call-p)
  "Creates ivy actions out of simple functions.

To create an ivy action and give it a keybinding, first write a
function that takes one string as an argument; return type is
immaterial. Then use

(define-key ivy-minibuffer-map KEYBINDING
  (my/make-ivy-action-doer #'your-function EXITING-CALL-P))

Where EXITING-CALL-P specifies whether the action should exit
ivy.
"
  (lambda ()
    (interactive)
    (ivy-set-action action)
    (if exiting-call-p (ivy-done) (ivy-call))))

(defun my/counsel-describe-action (x)
  (if lsp-mode
      (progn
        (imenu (cdr x))
        (lsp-describe-thing-at-point))
    (describe-symbol (intern x))))
(define-key ivy-minibuffer-map [remap kill-ring-save] (my/make-ivy-action-doer #'ivy--action-copy nil))
(define-key ivy-minibuffer-map (kbd "C-c C-d") (my/make-ivy-action-doer #'my/counsel-describe-action nil))

;; Make ivy prefer exact matches, even when using orderless
(defun my/ivy-recompute-index-prefer-match (_re-str cands)
  "Recompute index of selected candidate.
This function serves as a fallback when nothing else is available."
  (or (cl-position ivy-text cands :test #'string=) 0))
(setf (alist-get t ivy-index-functions-alist) 'my/ivy-recompute-index-prefer-match)

(defun my/company-common-or-ivy ()
    (interactive)
    (if (company--active-p) (counsel-company) (company-complete-common)))
