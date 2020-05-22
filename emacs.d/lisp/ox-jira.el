(org-export-define-derived-backend 'jira 'md
  :translate-alist '((bold . jira-bold)
                     (headline . jira-headline)
                     (plain-text . jira-plain)))

(defun org-export-jira ()
  (interactive)
  (org-export-to-buffer 'jira "*Org Jira*"))


(defun jira-bold (bold text info)
  (s-concat "*" text "*"))
(defun jira-plain (text info)
  (replace-regexp-in-string "\\(\.\\)\n\\(\.\\)" "\\1 \\2" text))
(defun jira-headline (headline contents info)
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))

         )
    (s-concat "h" (number-to-string level) ". " title "\n\n" contents)

    ))
(defun jira-list (_plain-list contents _info)

  )

(org-export-to-buffer 'jira "*Org Jira*")
""
(message (number-to-string 1))

(s-matches-p "^abc" "abcd")

(replace-regexp-in-string "\\(\.\\)\n\\(\.\\)" "\\1 \\2" "foo
bar")
(jira-plain "foo
bar" nil)

(defun count-leading-spaces (s)
  (let ((char (nth s ))))
  (while )
  )

(nth 1 "foo")
(s-count-matches-all "^\\(abc\\)*abc" "abcabcdabc")
