;; Setup package archives.  Install anything that is missing.
(require 'package)
(add-to-list 'package-archives
       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(if (not
     (let ((install-result (package-install-selected-packages t)))
     (and
      (stringp install-result)
      (string= install-result "All your packages are already installed"))))
    (progn (package-refresh-contents)
     (package-install-selected-packages t)))
