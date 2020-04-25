(setq custom-org-refile-targets
      '(
        (org-agenda-files . (:maxlevel . 2))
        (nil . (:level . 2))
       )
)

(setq custom-org-agenda-files
 (list "/tmp/webdav/work/work-todo.org" "/tmp/webdav/stray-thoughts.org" "/tmp/webdav/current_todo.org" "/tmp/webdav/work/interviews.org" org-default-notes-file))

(setq org-refile-targets (append org-refile-targets custom-org-refile-targets))
(setq org-agenda-files (append org-agenda-files custom-org-agenda-files))

(setq org-capture-templates
      '((
        "t"
        "Todo item"
        entry
        (file+headline "" "Tasks")
        "** TODO %?\n   :PROPERTIES:\n   :CAPTURE_TIME: %U\n   :CAPTURE_CONTEXT: %a\n   :END:")
       (
        "w"
        "Work item"
        entry
        (file+headline "/tmp/webdav/work/work-todo.org" "Unsorted")
        "** %^{Todo state|TODO|NEXT|WAITING} %?\n   :PROPERTIES:\n   :CAPTURE_TIME: %U\n   :CAPTURE_CONTEXT: %a\n   :END:"
        :jump-to-captured t
        )))
