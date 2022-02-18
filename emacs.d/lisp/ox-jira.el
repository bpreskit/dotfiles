(require 'ox-md)
(org-export-define-derived-backend 'jira 'md
  :translate-alist '((bold . jira-bold)
                     (italic . jira-italic)
                     (underline . jira-underline)
                     (strike-through . jira-strikethrough)
                     (headline . jira-headline)
                     (plain-text . jira-plain)
                     (plain-list . jira-list)
                     (src-block . jira-code)
                     (example-block . jira-code)
                     (fixed-width . jira-code)
                     (link . jira-link)
                     (verbatim . jira-verbatim)
                     (inline-src-block . jira-verbatim)
                     (code . jira-verbatim)
                     (horizontal-rule . jira-horizontal-rule)
                     (quote-block . jira-quote)
                     (subscript . jira-subscript))
  :options-alist '((:with-toc nil)))

(defun org-export-jira ()
  (interactive)
  (org-export-to-buffer 'jira "*Org Jira*" nil t))

;; Straightforward "markdown syntax" functions.
(defun jira-bold (_bold text info)
  (s-concat "*" text "*"))
(defun jira-italic (_italic contents _info)
  (format "_%s_" contents))
(defun jira-underline (_underline contents _info)
  (format "+%s+" contents))
(defun jira-strikethrough (_strikethrough contents _info)
  (format "-%s-" contents))
(defun jira-quote (_quote contents _info)
  (format "{quote} %s {quote}" (s-trim contents)))
(defun jira-plain (text info)
  (replace-regexp-in-string "\\(\.\\)\n\\(\.\\)" "\\1 \\2" text))
(defun jira-horizontal-rule (_rule _contents _info)
  "----")
(defun jira-verbatim (verbatim _contents _info)
  (let ((value (org-element-property :value verbatim)))
    (format "{{%s}}" value)))
(defun jira-subscript (_subscript contents _info)
  (format "_%s" contents))

;; Adapted from org-md-example-block in ox-md.el.
(defun jira-code (code-block _contents _info)
  (let* ((code (car (org-export-unravel-code code-block)))
         (name (org-element-property :name code-block))
         (title (if name (format ":title=%s" name) "")))
    (format "{noformat%s}\n%s\n{noformat}" title code)))

;; Process org headlines into h<k>, where k is the heading depth.  Note that the root heading (where
;; you invoke org-export-jira) is ignored, and subheadings will be processed as if they start from
;; level 1.
(defun jira-headline (headline contents info)
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info)))
    (s-concat "h" (number-to-string level) ". " title "\n\n" contents)))

;; Process links.  This function is adapted from org-md-link in ox-md.el.
(defun jira-link (link contents info)
  (let ((link-org-files-as-md
   (lambda (raw-path)
     ;; Treat links to `file.org' as links to `file.md'.
     (if (string= ".org" (downcase (file-name-extension raw-path ".")))
         (concat (file-name-sans-extension raw-path) ".md")
       raw-path)))
	(type (org-element-property :type link)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
           (org-export-resolve-fuzzy-link link info)
         (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
    (`plain-text			; External file.
     (let ((path (funcall link-org-files-as-md destination)))
       (if (not contents) (format "[%s]" path)
         (format "[%s|%s]" contents path))))
    (`headline
     (format
      "[%s|#%s]"
      ;; Description.
      (cond ((org-string-nw-p contents))
      ((org-export-numbered-headline-p destination info)
       (mapconcat #'number-to-string
            (org-export-get-headline-number destination info)
            "."))
      (t (org-export-data (org-element-property :title destination)
              info)))
      ;; Reference.
      (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
    (_
     (let ((description
      (or (org-string-nw-p contents)
          (let ((number (org-export-get-ordinal destination info)))
			(cond
       ((not number) nil)
       ((atom number) (number-to-string number))
       (t (mapconcat #'number-to-string number ".")))))))
       (when description
         (format "[%s|#%s]"
           description
           (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (let ((raw-path (org-element-property :path link)))
        (cond ((not (equal "file" type)) (concat type ":" raw-path))
        ((not (file-name-absolute-p raw-path)) raw-path)
        (t (expand-file-name raw-path)))))
      (caption (org-export-data
          (org-export-get-caption
           (org-export-get-parent-element link)) info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
      (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
         (path
		(cond
     ((member type '("http" "https" "ftp" "mailto"))
      (concat type ":" raw-path))
     ((string= type "file")
      (org-export-file-uri (funcall link-org-files-as-md raw-path)))
     (t raw-path))))
          (if (not contents) (format "[%s]" path)
      (format "[%s|%s]" contents path)))))))

(defun jira-list (_plain-list contents _info)
  (jira-process-list-contents contents))

;; Loop through the lines in contents, process them, and join them back together.
(defun jira-process-list-contents (contents)
  (let (processed-lines)
    (dolist (line (s-lines contents))
      (setq processed-lines (append processed-lines (list (jira-process-list-line line)))))
    (s-join "\n" processed-lines)))

;; This cheats a little bit: the contents come in processed in a markdown-like format, where the
;; depth is expressed by four leading spaces.  This, therefore, replaces each group of four spaces
;; with a dash (in Atlassian, number of leading dashes is the depth of the item).
(defun jira-process-list-line (line)
  (let* ((leading-ws (car (s-match "^\s+" line)))
         (len-leading-ws (length leading-ws))
         (level (+ 1 (/ len-leading-ws 4))))
    (s-replace-regexp "^[-\s]+" (s-concat (make-string level ?-) " ") line)))
