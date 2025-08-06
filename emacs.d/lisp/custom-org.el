;; -*- mode: Lisp; lexical-binding: t; -*-
(require 'company)
(require 'f)
(require 'org)
(require 'org-agenda)
(require 'ox-pandoc)
(require 's)
(require 'seq)

;; No completion, since it's plain text.
(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (company-mode -1)
              (display-line-numbers-mode -1))))

;; Misc org variables
(setq org-ellipsis "⤵"
      org-log-done (quote time)
      org-log-into-drawer t
      org-default-notes-file "~/notes.org"
      org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "DONE(d)")))
      org-adapt-indentation nil)

;; Make agenda files refileable
(setq custom-org-refile-targets
      '((org-agenda-files . (:maxlevel . 2))
        (nil . (:level . 2)))
      org-refile-targets (append org-refile-targets custom-org-refile-targets))

(dolist (target custom-org-refile-targets) (add-to-list 'org-refile-targets target))

;; Enable plantuml
;; You'll need the plantuml apt package for this
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
;; Customize the jar file like so, if you want new features.
;; (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml-1.2024.3.jar")

;; Org capture templates. `org-capture-templates'
(setq org-capture-templates
      '((
          "t"
          "Todo item"
          entry
          (file+headline "" "Captures")
          "** TODO %?\n   :PROPERTIES:\n   :CAPTURE_TIME: %U\n   :CAPTURE_CONTEXT: %a\n   :END:"
          :jump-to-captured t)
        (
         "c"
         "Plain capture"
         entry
         (file+headline "" "Captures")
         "** %?\n   :PROPERTIES:\n   :CAPTURE_TIME: %U\n   :CAPTURE_CONTEXT: %a\n   :END:"
         :jump-to-captured t)
        ("l"
         "Thing to look up later"
         entry
         (file+headline "/tmp/webdav/current_todo.org" "Lookups")
         "* TODO %^{Search term}
   :PROPERTIES:
   :CAPTURE_TIME: %U
   :CAPTURE_CONTEXT: %a
   :END:
   - [[ddg:%\\1][%\\1]]%?"
         :jump-to-captured t)))

;; Map special link types `org-link-abbrev-alist'.
(setq custom-org-link-abbrevs
      '(("ddg" . "https://duckduckgo.com/?q=")))

(setq org-link-abbrev-alist
      (append org-link-abbrev-alist custom-org-link-abbrevs))

;; Make emails linkable
;; Use https://github.com/garoose/copy-message-id extension to get the message id
(org-add-link-type "email" 'org-thunderbird-open)

(defun org-thunderbird-open (link)
  (start-process "thunderbird" nil "thunderbird" (concat "mid:" link)))

;; Set category of links where the link determines the description.
;; Do that.
(setq auto-desc-link-prefixes
      '("jira" "ddg" "man" "info"))

(defun description-from-link (link desc)
  "If no default description is apparent and `link' comes from
one of the special types in `auto-desc-link-prefixes', then form
a description by cutting off the '<type>:' prefix from `link'."
  (if (and (s-blank-p desc)
           (seq-find (lambda (x) (s-prefix-p x link)) auto-desc-link-prefixes))
      (s-trim-right (s-chop-prefix (car (s-match ".*?:" link)) link))
    desc))

(setq org-link-make-description-function 'description-from-link)

;; Make man: links parseable in org-mode.
;; Lot of machinery here, I expected more things to just use `browse-url'
(setq prefix-list '("man"))

(defun add-prefix-to-links (prefix)
	"Use org-link-set-parameters to make links prefixed by `prefix`
get opened with `browse-url`."
	(let ((prefix prefix))
		(let
				((follow
					(lambda (path) (browse-url (concat prefix ":" path)))))
			(org-link-set-parameters prefix
                               :follow follow))))

(dolist (prefix prefix-list) (add-prefix-to-links prefix))

(defun org-source ()
  (interactive)
  (org-insert-structure-template "src"))

(load "ox-jira" t)

;; Sort by todo status and priority
(setq org-lowest-priority ?F)
(defun my-org-get-priority (S)
  "Get priority, but use 1 instead of 1000 for unlabeled headings."
  (if (s-matches-p org-priority-regexp S)
      (org-get-priority S)
      1))

(defun my-get-priority-key ()
  "Get the sorting key (based on custom priority) for the heading at point."
  (my-org-get-priority (org-get-heading)))

(defun my-cmp-priority (a b)
  "Compare priority of two strings."
  (let ((a-priority (my-org-get-priority a))
        (b-priority (my-org-get-priority b)))

     (if (= a-priority b-priority)
         nil
       (if (> a-priority b-priority) 1 -1))))

(defun my-org-sort (&optional arg)
  "Sort by priority and todo order. With prefix argument, sorts parent of this subtree."
  (interactive "P")
  (if arg (org-up-element))
  (org-sort-entries nil ?A nil nil nil nil)
  (org-sort-entries nil ?f 'my-get-priority-key '>= nil nil)
  (org-sort-entries nil ?o nil nil nil nil))

(setq org-after-refile-insert-hook
      '(lambda () (org-up-heading-safe) (my-org-sort) (save-buffer)))
(setq org-after-sorting-entries-or-items-hook
      '(lambda () (progn (org-overview) (org-reveal) (org-show-children))))

(defun my/counsel-org-goto (&optional prefix-p)
  (interactive "P")
  (if prefix-p (counsel-org-goto-all) (counsel-org-goto)))
