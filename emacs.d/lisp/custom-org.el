;; -*- mode: Lisp; lexical-binding: t; -*-
(require 'f)
(require 'org)
(require 'org-agenda)
(require 's)

;; Misc org variables
(setq org-ellipsis "⤵"
      org-directory
      (if (and (f-writable? "/tmp/webdav") (f-dir-p "/tmp/webdav"))
          "/tmp/webdav"
        "~/notes")
      org-default-notes-file (s-join "/" (list org-directory "captures.org"))
      org-log-done (quote time)
      org-log-into-drawer t
      org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "DONE(d)")))
      org-adapt-indentation nil)

;; Make agenda files refileable
(setq custom-org-refile-targets
      '((org-agenda-files . (:maxlevel . 2))
        (nil . (:level . 2)))
      org-refile-targets (append org-refile-targets custom-org-refile-targets))

;; Set up my webdavs as agenda-files
(setq custom-org-agenda-files
      (list "/tmp/webdav/work/work-todo.org"
            "/tmp/webdav/stray-thoughts.org"
            "/tmp/webdav/current_todo.org"
            "/tmp/webdav/work/interviews.org"
            "/tmp/webdav/bike/svbc.org"
            org-default-notes-file))
(dolist (file custom-org-agenda-files) (add-to-list 'org-agenda-files file))
(dolist (target custom-org-refile-targets) (add-to-list 'org-refile-targets target))

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
         :jump-to-captured t)
       (
        "w"
        "Work item"
        entry
        (file+headline "/tmp/webdav/work/work-todo.org" "Unsorted")
        "** %^{Todo state|TODO|NEXT|WAITING} %?\n   :PROPERTIES:\n   :CAPTURE_TIME: %U\n   :CAPTURE_CONTEXT: %a\n   :END:"
        :jump-to-captured t)))

;; Some helper functions that can filter agenda files
(defun get-non-work-files ()
  (seq-filter '(lambda (x) (not (s-match "work" x))) (org-agenda-files)))

(defun get-work-files ()
  (seq-filter '(lambda (x) (s-match "work" x)) (org-agenda-files)))

;; Create my agenda views. See `org-agenda-custom-commands'.
(setq custom-org-agenda-commands
      '(("n" "Next items" todo "NEXT")
        ("h" . "Home agendas")
        ("ht" todo "" ((org-agenda-files (get-non-work-files))))
        ("hn" todo "NEXT" ((org-agenda-files (get-non-work-files))))
        ("ha" "Home agenda and next"
         ((agenda "")
          (todo "NEXT" ((org-agenda-overriding-header "Home next items"))))
         ((org-agenda-files (get-non-work-files))))
        ("hA" "Home agenda and all todo"
         ((agenda "")
          (todo "" ((org-agenda-overriding-header "Home todo items"))))
         ((org-agenda-files (get-non-work-files))))
        ("w" . "Work agendas")
        ("wt" "All work todo's" todo "" ((org-agenda-files (get-work-files))))
        ("wn" "All work NEXT" todo "NEXT" ((org-agenda-files (get-work-files))))
        ("wa" "Work agenda and NEXT"
         ((agenda "")
          (todo "NEXT" ((org-agenda-overriding-header "Work next items"))))
         ((org-agenda-files (get-work-files))))
        ("wA" "Work agenda and all todo"
         ((agenda "")
          (todo "" ((org-agenda-overriding-header "Work todo items"))))
         ((org-agenda-files (get-work-files))))))

(setq org-agenda-custom-commands custom-org-agenda-commands)

;; Map special link types `org-link-abbrev-alist'.
(setq custom-org-link-abbrevs
      '(("ddg" . "https://duckduckgo.com/?q=") ("jira" . "https://jira.purestorage.com/browse/")))

(setq org-link-abbrev-alist
      (append org-link-abbrev-alist custom-org-link-abbrevs))

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

;; Make slack:// style links go straight into desktop,
;; even if they were specified with https://.

;; Team ID for slack's API.
(defconst purestorage-slack-team-id "T02UZQP39")

;; Helpers to construct slack:// URL
(defun slack-link-from-components (channel ts)
  (s-concat "slack://channel?" "team=" purestorage-slack-team-id "&"
            "id=" channel
            (if ts "&message=") (slack-format-ts ts)))

(defun slack-format-ts (ts)
  "E.g., 'p1556311700001200' -> '1556311700.001200'"
  (if ts (let
             ((no-p-ts (s-chop-prefix "p" ts)))
           (s-join "." (list (subseq no-p-ts 0 -6) (subseq no-p-ts -6))))))

;; Transform https:// link into slack:// link.
(defun process-slack-link (link)
  "Turn a link from Slack's 'Copy Link' think into a proper
`slack://`-style deep link."
  (interactive)
  (let* ((components (s-split "/" (s-chop-prefix (car (s-match ".*archives/" link)) link) t))
         (channel (car components))
         (ts (car (cdr components))))
    (slack-link-from-components channel ts)))

;; Idempotently transform to slack://
(defun insert-or-process-slack-link (tag)
  (if (s-contains-p "purestorage.slack.com" tag)
      (process-slack-link tag)
    (s-concat (if (not (s-prefix-p "slack://" tag)) "slack://") tag)))

;; Put it into org!
(let ((follow
       (lambda (url) (browse-url (insert-or-process-slack-link url)))))
  (org-link-set-parameters "slack" :follow follow))

(defun org-source ()
  (interactive)
  (org-insert-structure-template "src"))

(load "ox-jira" t)

;; Sort by todo status and priority
(defun my-org-get-priority (S)
  "Get priority, but use 1 instead of 1000 for unlabeled headings."
  (if (s-matches-p org-priority-regexp S)
      (org-get-priority S)
      1))

(defun my-get-priority-key ()
  "Get the sorting key (based on custom priority) for the heading at point."
  (my-org-get-priority (org-get-heading)))

(defun my-org-sort ()
  "Sort by priority and todo order."
  (interactive)
  (progn
    (org-sort-entries nil ?f 'my-get-key '>= nil nil)
    (org-sort-entries nil ?o nil nil nil nil)))
