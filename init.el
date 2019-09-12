
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

(use-package dash)

;; Functions (load all files in defuns-dir)
(require 'cl)
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

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
(require 'setup-eww)

(require 'use-dired)
(require 'key-bindings)
(require 'use-ess)
(require 'use-julia)
(require 'use-org)
(require 'use-ivy)
(require 'use-magit)
;; (require 'use-exwm)
;; (require 'use-slack)
(require 'use-markdown-mode)
(require 'use-ein)
(require 'use-yasnippet)
(require 'use-python)
(require 'use-haskell)
(require 'use-clojure)

(use-package define-word)
(use-package which-key
  :config (which-key-mode))
(use-package transpose-frame
  :bind ("M-t f" . transpose-frame))
;; (use-package nxhtml)
(use-package pretty-mode
  :config
  (pretty-add-keywords 'org-mode
                       '(("^ *\\(#\\+begin_src\\) " . ?λ))))

;; Support for editing text areas in Chrome.  This requires a Chrome extension
;; to be installed.  The two options that I found are GhostText and Atomic
;; Chrome.  I've only tried GhostText.
(use-package atomic-chrome
  :config (atomic-chrome-start-server))

;; yaml files
(use-package yaml-mode)

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
;; Things that I may or may not want to use
;; ======================================================================

;; (use-package inline-string-rectangle)
;; (require 'delsel)
;; (require 'jump-char)
;; ;; (require 'eproject)
;; (require 'wgrep)
;; (require 'smart-forward)
;; (require 'change-inner)
;; (require 'multifiles)

;; ;; Run at full power please
;; (put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
