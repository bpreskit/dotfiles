;; -*- lexical-binding: t -*-
;; Make modes happen.
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . ansible))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Use json-mode on json files.
(setq auto-mode-alist (remove '("\\.json\\'" . javascript-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(setq js-indent-level 2)

;; Global modes
;; Do some stuff to set up smartparens.
(use-package smartparens
  :custom
  (sp-escape-quotes-after-insert nil)
  (sp-autoescape-string-quote nil)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-pair "\"" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "\\\"" nil :actions :rem)
  )

;; Auto-complete
(use-package company
  :init
  (global-company-mode)
)

;; Monitor whitespace problems
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing empty space-after-tab space-before-tab))
)
(setq-default indent-tabs-mode nil)

;; Some random hooks.
(add-hook 'find-file-hook
          (lambda ()
            (progn
              (linum-mode-ifnt-log)
              (if (and
           (stringp (buffer-file-name))
           (string-match ".*\\(\.\\|\\(sys\\)\\)log.*\\(\.tar\\)?\\(\.gz\\)?" (buffer-file-name)))
                  (progn
                    (read-only-mode)
                    (show-smartparens-mode -1))))))
(use-package all-the-icons
  :if my/use-all-the-icons
  :config
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts)))
(use-package all-the-icons-dired
  :if (and my/use-all-the-icons my/use-all-the-icons-dired)
  :hook dired-mode
)
(use-package all-the-icons-ivy
  :if (and my/use-all-the-icons my/use-all-the-icons-ivy)
  :after (ivy)
  :custom (all-the-icons-spacer . ("  "))
  :init (all-the-icons-ivy-setup))
(use-package marginalia
  :custom (marginalia-align 'left)
  :init (marginalia-mode)
)
(use-package hl-line
  :init (global-hl-line-mode)
  )
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay . (0.5))
  )

;; Octave stuff.
(add-hook 'octave-mode-hook
		(lambda () (progn (setq octave-comment-char ?%)
					(setq comment-start "% ")
					(setq comment-add 0))))

;; Not sure what hide-ifdef is for.
(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)

;; ediff setup
(add-hook 'ediff-before-setup-hook 'save-ediff-before-windows)
(add-hook 'ediff-quit-hook 'restore-ediff-before-windows)

;; ansible
(require 'ansible)
(require 'ansible-doc)

;; projectile
(use-package projectile
  :config
  (projectile-mode)
  )

;; magit
(use-package magit)
(use-package magit-delta
  :after magit
  :custom
  (magit-delta-default-dark-theme "Monokai Extended")
  (magit-delta-default-light-theme "Github")
  (magit-delta-hide-plus-minus-markers nil)
  :hook (magit-mode . magit-delta-mode))

;; Turn off scroll bars, menu bars, tool bars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(use-package inhibit-mouse
  :ensure t
  :custom
  (inhibit-mouse-button-numbers '(1 2 3 4 5))
  :config
  (inhibit-mouse-mode 1))

;; Set up JS(X) autocomplete
(add-to-list 'load-path "/path/to/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)

(add-hook 'before-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; Set fonts
;; Source them from
;;   https://fonts.google.com/specimen/Roboto+Mono
;;   https://fonts.google.com/specimen/Fira+Code
;;   https://fonts.google.com/specimen/Inter
;;   https://fonts.google.com/specimen/Source+Sans+3
;;   https://github.com/edwardtufte/et-book
;; Copy the ttf's into ~/.local/share/fonts
(defun my/set-default-fonts (&optional fonts_)
  (let ((fonts (if fonts_ fonts_ (font-family-list))))
  (when (member "Roboto Mono" fonts)
    (set-face-attribute 'default nil :family "Roboto Mono" :height 150)
    (set-face-attribute 'fixed-pitch nil :family "Roboto Mono" :height 0.9))
  (when (member "Fira Code" fonts)
    (set-face-attribute 'default nil :family "Fira Code" :height 150)
    (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 0.9))
  (when (member "Inter" fonts)
    (set-face-attribute 'variable-pitch nil :family "Inter" :height 1.18))
  (when (member "Source Sans 3" fonts)
    (set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 1.18))
  (when (member "ETBembo" fonts)
    (set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.18))))

;; Fonts aren't loaded at emacs.service time, so add them later.
(defun my/set-default-fonts-and-dip ()
  "Set default font on first graphical frame creation."
  (let ((fonts (font-family-list)))
        (if fonts
            (progn
              (my/set-default-fonts fonts)
              (setq after-make-frame-functions (remove #'my/set-default-fonts-and-dip after-make-frame-functions))))))

(setq after-make-frame-functions (append after-make-frame-functions (my/set-default-fonts-and-dip)))

;; Get ligatures going for Fira Code
(use-package ligature
  :config
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; ;; www wwww
                          ;; ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
