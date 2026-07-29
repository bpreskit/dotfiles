;; Basic setup for lsp-mode
(use-package lsp-mode
  :custom
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  (lsp-idle-delay 0.1)
  (eldoc-idle-delay 0)
  :hook
  ((go-mode rust-mode java-mode python-mode c-mode c++-mode protobuf-mode)
   .
   lsp-deferred)
  :bind
  (:map
   lsp-mode-map
   ("C-c C-d" . lsp-describe-thing-at-point)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-references] . lsp-find-references)
   :map
   lsp-signature-mode-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("M-<right>" . lsp-signature-next)
   ("M-<left>" . lsp-signature-previous)
   :map
   lsp-command-map
   ("a r" . lsp-rename)))
(use-package lsp-ivy
  :ensure t
  :bind
  (:map
   lsp-mode-map
      ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)))

;; lsp-ui setup
(defun my/lsp-ui-doc-unfocus-and-describe ()
  "Unfocus from lsp-ui-doc frame and describe"
  (interactive)
  (lsp-ui-doc-unfocus-frame)
  (lsp-describe-thing-at-point))
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-eldoc-enable-hover nil)
  :bind
  (:map
   lsp-ui-mode-map
   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . lsp-ui-peek-find-references)
   :map
   lsp-command-map
   ("h o" . lsp-ui-doc-focus-frame)
   :map
   lsp-ui-doc-frame-mode-map
   ("d" . my/lsp-ui-doc-unfocus-and-describe)))
(add-hook 'lsp-mode-hook
          'yas-minor-mode-on)

;; Set up language-specific stuff.
(dolist (lib
     (list
      "cpp-conf"
      "golang-conf"
      "java-conf"
      "python-conf"
      "rust-conf"
      ;; "augment-conf"
      ))
  (load-library lib))

(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]site-packages\\'")

;; Fix imenu'ing with counsel in lsp, particularly in python
(define-advice counsel--imenu-candidates (:around (fn) prefer-lsp)
  "Don't let counsel replace the LSP imenu index with python.el's."
  (if (bound-and-true-p lsp-mode)
      ;; The `cond' in counsel only checks `major-mode'; `imenu--make-index-alist'
      ;; still uses the buffer-local `imenu-create-index-function' (LSP).
      (let ((major-mode 'fundamental-mode))
        (funcall fn))
    (funcall fn)))

;; 1. Remember where a parent symbol lives, since lsp-mode discards it.
(define-advice lsp--symbol-to-hierarchical-imenu-elem (:around (fn sym) keep-parent-pos)
  (let ((elem (funcall fn sym)))
    (if (imenu--subalist-p elem)
        (cons (propertize (car elem) 'counsel-imenu-pos
                          (ht-get lsp--line-col-to-point-hash-table
                                  (lsp--get-line-and-col sym)))
              (cdr elem))
      elem)))

;; 2. Same as counsel's version, plus a candidate for each positioned parent.
(define-advice counsel-imenu-get-candidates-from (:override (alist &optional prefix) include-parents)
  (cl-flet ((make-key (name)
              (concat (when prefix
                        (concat (propertize prefix 'face 'ivy-grep-info) ": "))
                      name)))
    (cl-mapcan
     (lambda (elm)
       (if (imenu--subalist-p elm)
           (let* ((name (car elm))
                  (pos (get-text-property 0 'counsel-imenu-pos name))
                  (key (make-key name)))
             (append
              (when pos (list (cons key (cons key pos))))
              (counsel-imenu-get-candidates-from
               (cl-loop for (e . v) in (cdr elm) collect
                        (cons e (if (integerp v) (copy-marker v) v)))
               (concat prefix (if prefix ".") name))))
         (let ((key (make-key (car elm))))
           (list (cons key
                       (cons key (if (overlayp (cdr elm))
                                     (overlay-start (cdr elm))
                                   (cdr elm))))))))
     alist)))
