
;; ======================================================================
;; Disable certain GUI functionality
;; ======================================================================

;; Turn off mouse interface early in startup to avoid momentary display

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; ======================================================================
;; Set up the environment (directory structure, etc.)
;; ======================================================================

(setq home-dir (getenv "HOME"))

;; Give names to (some of) the subdirectories.
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; (setq site-lisp-dir
;;       (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path (expand-file-name
                         "other-packages" user-emacs-directory))
;; (add-to-list 'load-path site-lisp-dir)

;; ;; Add external projects to load path
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file
      (expand-file-name "custom.el" settings-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(save-place-mode 1)
(setq save-place-file
      (expand-file-name ".places" settings-dir))

;; ======================================================================
;; Working on a mac?
;; ======================================================================
;;
;; Macs need special treatment...because they're special.

(setq is-mac (equal system-type 'darwin))

(if is-mac
    (require 'mac))

;; ======================================================================
;; Load and initialize packages (this contains most of the settings)
;; ======================================================================

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Enable the package manager and use-package.
(require 'setup-use-package)

;; A modern list API.
(use-package dash)

;; The long lost Emacs string manipulation library.
(use-package s)

(use-package string-inflection)

;; Functions (load all files in defuns-dir)
(require 'cl)
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'compat)

;; Load my set-up files
(require 'appearance)
(require 'custom)
(require 'sane-defaults)
(require 'my-misc)

(require 'use-multiple-cursors)
(require 'use-expand-region)
(require 'use-tramp)
(require 'use-god-mode)
(require 'use-browse-kill-ring)
(require 'setup-hippie)
;; (require 'setup-eww)         ; eww is slow to load, and I don't really use it.

(require 'use-dired)
(require 'key-bindings)
;; (require 'use-ess)
(require 'use-julia)
(require 'use-org)
(require 'use-ivy)
;; (require 'use-vertico) ; replacing ivy with vertico, marginalia, consult, embark, etc.
(require 'use-magit)
;; (require 'use-exwm)
;; (require 'use-slack)
(require 'use-markdown-mode)
;; (require 'use-ein)
(require 'use-yasnippet)
(require 'use-python)
;; (require 'use-haskell)
(require 'use-clojure)
;; (require 'use-paredit)
;; (require 'use-meow)
;; (use-package boon)
;; (require 'use-boon)

(require 'setup-system-interaction)

;; (use-package define-word
;;   :defer t)

;; ;; Use the built-in dictionary on Macs.
;; (use-package osx-dictionary
;;   :defer t)

(use-package which-key
  :config (which-key-mode))

(use-package transpose-frame
  :bind ("M-t f" . transpose-frame))
;; (use-package nxhtml)

;; (use-package pretty-mode
;;   :defer t
;;   :config
;;   (pretty-add-keywords 'org-mode
;;                        '(("^ *\\(#\\+begin_src\\) " . ?λ))))

;; Support for editing text areas in Chrome.  This requires a Chrome extension
;; to be installed.  The two options that I found are GhostText and Atomic
;; Chrome.  I've only tried GhostText.
(use-package atomic-chrome
  :config (atomic-chrome-start-server))

;; yaml files
(use-package yaml-mode
  :defer t)

;; Timing start-up time
(use-package esup
  :defer t)

;; ack from emacs
(use-package ack
  :pin gnu
  :ensure t

  :init
  (setq ack-defaults-function 'ack-legacy-defaults)
  ;; (defun zjr/ack-buffer-name-function (name-of-mode)
  ;;   (concat "**" (prin1-to-string major-mode) "**"))
  (setq ack-buffer-name-function nil)

  :bind (("M-s a" . ack))
  :bind (:map dired-mode-map
              ("M-s a" . ack))

  :bind (:map ack-mode-map
              ("n" . compilation-next-error)
              ("p" . compilation-previous-error)
              ("F" . next-error-follow-minor-mode)))

(use-package ag)
(use-package transient
  :defer t)

;; Auto-completion
(use-package company
  :config (global-company-mode))

;; Keep expresssions aligned (especially in lisp-like modes).
(use-package aggressive-indent
  :defer t)

;; Work with CSV files.
(use-package csv-mode
  :pin gnu
  :defer t)

;; Diagramming with plantuml.
(use-package plantuml-mode
  :defer t
  :init
  (setq plantuml-server-url "http://localhost:8080"))

(use-package flycheck-plantuml :defer t)

;; Edit Jenkinsfile files
(use-package jenkinsfile-mode
  :defer t)

;; Edit Dockerfile files
(use-package dockerfile-mode
  :defer t)

(use-package terraform-mode :defer t)

(use-package org-iv
  :defer t
  :commands org-iv/immediate-view
  :config
  (org-iv/add-to-alist
   'org-iv/config-alist
   `(("config-001"
      :front-html-file ,(expand-file-name "default/org-iv-front-file.html" org-iv/root)
      ;; The file put on front of the html generated by org-file.
      :back-html-file ,(expand-file-name "default/org-iv-back-file.html" org-iv/root)
      ;; The file put on back of the html generated by org-file.
      :web-test-root ,(expand-file-name "default" org-iv/root)
      ;; where we copy the content of web-resource-dir into
      :web-test-port 9876))))

(use-package avy
  :defer t
  :bind (("M-g c" . avy-goto-char)
         ("M-g M-c" . avy-goto-char-2)
         ("M-g M-g" . avy-goto-char-timer)))

(use-package hydra
  :defer t
  :init
  (defcustom hydra-select-by-mode-list
    '()
    "Specify a default hydra to use in each given mode.")

  (defun hydra-autoselect ()
    (interactive)
    (let ((hydra-func (alist-get major-mode hydra-select-by-mode-list)))
      (when hydra-func
        (funcall hydra-func))))

  :bind (("M-h" . hydra-autoselect)))

(use-package projectile
  :defer t
  :bind (("C-c f" . projectile-find-file)))

(use-package counsel-projectile
  :defer t)

;; (use-package elfeed
;;   :defer t
;;   :init
;;   (setq elfeed-feeds
;;         '("https://www.reddit.com/r/CryptoCurrency/.rss"
;;           "https://www.reddit.com/r/USDC/.rss")))
;; (use-package elfeed-web :defer t)

(require 'zoom-frm)

(use-package fullframe)

(use-package fold-this
  :defer t
  ;; :init (setq fold-this-overlay-text "[[..]]")
  )

(use-package origami
  :defer t
  :init (global-origami-mode 1)
  :bind (("M-o" . origami-recursively-toggle-node)
         ("M-O" . origami-toggle-all-nodes)
         ;; ("M-t M-o TAB" . origami-recursively-toggle-node)
         ))
;; (use-package vimish-fold)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package symbol-overlay
  :defer t)

(use-package alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

(use-package wc-mode :defer t)

(use-package mermaid-mode :defer t)

(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/Documents/obsidian")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-link'
              ("C-c C-l" . obsidian-insert-wikilink)))

;; (use-package popper
;;   :ensure t ; or :straight t
;;   :bind (("C-`"   . popper-toggle)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;;           clojure-mode
;;           help-mode
;;           compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

;; (use-package shackle
;;   :config
;;   (setq shackle-default-rule nil
;;         ;; '(:other t :select t :inhibit-window-quit t)
;;         )
;;   (setq shackle-rules ;; nil
;;         '(;; (help-mode :select t)
;;           ;; ("^\\*Help\\*$" :regexp t)
;;           ;; ("*Help*" :select t)
;;           (clojure-mode :other t)
;;           )
;;         )
;;   ;; (setq shackle-default-rule '(:same t))
;;   ;; (setq shackle-rules '((clojure-mode :other t :select t)))
;;   ;; (setq shackle-rules '((clojure-mode :other t)))
;;   )

(use-package easysession
  :ensure t
  :custom
  ;; Interval between automatic session saves
  (easysession-save-interval (* 10 60))
  ;; Make the current session name appear in the mode-line
  (easysession-mode-line-misc-info t)
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 102))

;; (use-package kotlin-mode)

;; ;; A pomodoro timer
;; (use-package pomidor
;;   :config
;;   (setq pomidor-sound-tick nil
;;         pomidor-sound-tack nil))

;; Group buffers in the buffer list.  Or maybe not.  I'm not sold on this yet.
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("coding" (or (mode . python-mode)
                             (mode . julia-mode)
                             (mode . clojure-mode)
                             (mode . clojure-ts-mode)
                             (mode . sh-mode)
                             (mode . terraform-mode)))
               ("org" (or (mode . org-mode)
                          (mode . org-agenda-mode)))
               ("dired-mode" (mode . dired-mode))
               ("config files (elisp)" (mode . emacs-lisp-mode))
               ("scratch buffers" (name . "^\\*scratch.*\\*$"))
               ("helper buffers" (name . "^\\*.*\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; ;; flycheck for syntax checking
;; (use-package flycheck
;;   :init
;;   (add-hook 'after-init-hook #'global-flycheck-mode))
;; ;; (use-package flycheck-julia
;; ;;   :after flycheck
;; ;;
;; ;;   :init
;; ;;   (add-to-list 'flycheck-global-modes 'julia-mode)
;; ;;   (add-to-list 'flycheck-global-modes 'ess-julia-mode)
;; ;;
;; ;;   :config
;; ;;   (flycheck-julia-setup))

;; (use-package hledger-mode)

;; ;; Mathematica (Wolfram)
;; (use-package wolfram-mode
;;   :config
;;   (setq wolfram-path "/opt/homebrew/bin/wolframscript")
;;   (setq wolfram-program
;;         "/Applications/Wolfram Engine.app/Contents/Resources/Wolfram Player.app/Contents/MacOS/WolframKernel"))
;;
;; (autoload 'wolfram-mode "wolfram-mode" nil t)
;; (autoload 'run-wolfram "wolfram-mode" nil t)
;;
;; (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))
;; (setq wolfram-path "direcotry-in-Mathematica-$Path") ;; e.g. on Linux "~/.Mathematica/Applications"

;; ADDITIONAL PACKAGES (used in key-bindings.el)
;;     buf-move
;; DONE
;;     appearance.el
;;     setup-use-package.el
;;     sane-defaults.el
;;     custom.el
;;     mode-mappings.el
;;     my-misc.el
;;     setup-dired.el
;;     setup-org.el
;;     use-ivy.el
;;     .emacs.d/defuns/*.el
;;     use-julia.el
;;     use-magit.el
;; TO-DO
;;     setup-email.el
;;     setup-latex.el
;;     setup-markdown-mode.el
;;     setup-matlab-mode.el
;;     setup-shell.el
;;     setup-yasnippet.el
;; MAYBE
;;     setup-elfeed.el
;;     setup-elnode.el
;;     setup-eww.el
;;     setup-hippie.el
;;     setup-ido.el
;;     setup-mu4e.el
;;     setup-perspective.el
;; PROBABLY NOT
;;     setup-clojure-mode.el
;;     setup-ffip.el
;;     setup-html-mode.el
;;     setup-js2-mode.el
;;     setup-paredit.el
;;     setup-rgrep.el
;;     setup-ruby-mode.el
;;     setup-slime-js.el

;; ======================================================================
;; Stuff just for my work at Crescent
;; ======================================================================

(require 'crescent)
;; (crescent-set-env "dev")

;; ======================================================================
;; Things that I may or may not want to use
;; ======================================================================

;; (use-package inline-string-rectangle)
;; (require 'delsel)
;; (require 'jump-char)
;; ;; (require 'eproject)
;; (require 'wgrep)
;; (require 'smart-forward)
;; (require 'change-inner)
(use-package multifiles)

;; ;; Run at full power please
;; (put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
