;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; ;; Set path to dependencies
;; (setq site-lisp-dir
;;       (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
;; (add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path
             (expand-file-name "settings" user-emacs-directory))
;; (add-to-list 'load-path site-lisp-dir)

;; ;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))
;; (add-to-list 'load-path user-settings-dir)

;; ;; Add external projects to load path
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))

(add-to-list 'load-path
             "~/.emacs.d/other-packages/org-protocol-capture-html/")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" settings-dir))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" settings-dir))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup elnode before packages to stop it from starting a server
;;(require 'setup-elnode)

;; Setup packages
(require 'setup-package)
;; (require 'setup-use-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'exec-path-from-shell melpa)
   (cons 'magit                melpa)
   (cons 'paredit              melpa)
   (cons 'move-text            melpa)
   (cons 'gist                 melpa)
   (cons 'htmlize              melpa)
   (cons 'elisp-slime-nav      melpa)
   ;; (cons 'git-commit-mode      melpa)
   (cons 'gitconfig-mode       melpa)
   (cons 'gitignore-mode       melpa)
   (cons 'cider                melpa)
   (cons 'ido-completing-read+ melpa)
   (cons 'yasnippet            melpa)
   (cons 'buster-snippets      melpa)
   (cons 'perspective          melpa)
   (cons 'find-file-in-project melpa)
   (cons 'ess                  melpa)
   (cons 'dash                 melpa)
   (cons 's                    melpa)
   (cons 'expand-region        melpa)
   (cons 'mark-more-like-this  marmalade)
   (cons 'mark-multiple        melpa)
   (cons 'multiple-cursors     melpa)
   (cons 'jump-char            melpa)
   (cons 'wgrep                melpa)
   (cons 'smart-forward        melpa)
   (cons 'change-inner         melpa)
   (cons 'multifiles           melpa)
   (cons 'browse-kill-ring     melpa)
   (cons 'smex                 melpa)
   (cons 'smex                 marmalade)
   (cons 'tagedit              melpa)
   (cons 'buffer-move          melpa)
   (cons 'smooth-scrolling     melpa)
   (cons 'undo-tree            melpa)
   (cons 'dired-details        melpa)
   (cons 'shell-command        melpa)
   (cons 'eproject             melpa)
   (cons 'fill-column-indicator melpa)
   (cons 'zoom-frm             melpa)
   (cons 'visual-fill-column   melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac (exec-path-from-shell-initialize))

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)
(require 'setup-paredit)
(require 'ess-site)
;;(require 'setup-latex)
(require 'setup-elfeed)

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))
(eval-after-load 'matlab-mode '(require 'setup-matlab-mode))
(eval-after-load 'ess-julia '(require 'setup-julia-mode))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-mode/")
;; (load-library "matlab-load")

(setq inferior-julia-program-name "/home/zroth/bin/julia")

;; Map files to modes
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'mark-more-like-this)
(require 'inline-string-rectangle)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
;; (require 'eproject)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)

;; Forward/inverse search in LaTeX
(load "auctex.el" nil t t)
(setq TeX-engine 'luatex)
;; (eval-after-load 'tex-mode '(TeX-source-correlate-mode))

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; ;; Helm is supposed to be great for completion.
;; (require 'helm)
;; (require 'helm-config)
;; (helm-mode 1)
;; ;; (require 'helm-fuzzier)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Misc
(require 'appearance)
;; (require 'spacemacs-dark-theme)
(require 'my-misc)
(when is-mac (require 'mac))

;; ;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; Email, baby
(add-to-list 'load-path "~/programs/mu/mu4e")
(require 'setup-mu4e)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Diminish modeline clutter
(require 'diminish)

;; Web browsing
(require 'setup-eww)

;; org-protocol
(require 'org-protocol)
(require 'org-protocol-capture-html)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/inbox.org")
         "* TODO %?\n  %i\n  %a")
        ("a" "Article" entry (file+olp "~/Dropbox/org/lists.org" "Reading" "Internet articles")
         "
* %c
:PROPERTIES:
  :CAPTURED: %U
:END:
%?
%:initial")
        ;; ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
        ;;  "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Job posting" entry (file+olp "~/Dropbox/org/projects.org"
                                           "Job search" "Postings" "Apply")
         "
* %?%c
:PROPERTIES:
  :TITLE:        %^{Job title}
  :COMPANY:      %^{Company name}
  :LOCATION:     %^{Job location}
  :CAPTURED:     %U
  :POSTED:       %^{Job posting found at}
  :DATE_APPLIED:
:END:
%:initial
** Information
** Cover letter
** Follow up about application
")))

;; ;; helm
;; (use-package helm
;;   :diminish helm-mode
;;   :init (progn
;;           (require 'helm-config)
;;           ;; (require 'helm-org-rifle)
;;           ;; (require 'helm-orgcard)
;;           (setq helm-candidate-number-limit 100)
;;           ;; From https://gist.github.com/antifuchs/9238468
;;           (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;                 helm-input-idle-delay 0.01  ; this actually updates things
;;                                         ; reeeelatively quickly.
;;                 helm-yas-display-key-on-candidate t
;;                 helm-quick-update t
;;                 helm-M-x-requires-pattern nil
;;                 helm-ff-skip-boring-files t)
;;           (helm-mode))
;;   :bind (("C-c h" . helm-mini)
;;          ("C-h a" . helm-apropos)
;;          ("C-x b" . helm-buffers-list)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x)
;;          ("C-x c o" . helm-occur)
;;          ("C-x c s" . helm-swoop)
;;          ("C-x c y" . helm-yas-complete)
;;          ("C-x c Y" . helm-yas-create-snippet-on-region)
;;          ("C-x c b" . my/helm-do-grep-book-notes)
;;          ("C-x c SPC" . helm-all-mark-rings)))
;; (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")

  :bind (:map ivy-mode-map
              ("C-'" . ivy-avy))

  :config
  (progn
    (ivy-mode 1)
    ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    (setq ivy-height 10)
    ;; does not count candidates
    (setq ivy-count-format "")
    ;; no regexp by default
    (setq ivy-initial-inputs-alist nil)
    ;; configure regexp engine.
    (setq ivy-re-builders-alist
          ;; allow input not in order
          '((t   . ivy--regex-ignore-order)))

    (use-package ivy-rich
      :init
      (ivy-set-display-transformer 'ivy-switch-buffer
                                   'ivy-rich-switch-buffer-transformer)))

  :bind (("C-x b"     . ivy-switch-buffer)
         ("M-y"       . counsel-yank-pop)
         ("M-x"       . counsel-M-x)
         ("C-s"       . swiper)
         ("C-x C-f"   . counsel-find-file)
         :map org-mode-map
         ("C-c C-j"   . counsel-org-goto)
         ;("C-h a"     . helm-apropos)
         ;("C-x b"     . helm-buffers-list)
         ;("C-x c o"   . helm-occur)
         ;("C-x c SPC" . helm-all-mark-rings)
         ))

;; god-mode
(require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

;; which-key
(use-package which-key
  :config
  (which-key-mode t))

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
(put 'dired-find-alternate-file 'disabled nil)
