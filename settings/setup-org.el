(require 'ox-pandoc)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (org-update-parent-todo-statistics))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Indent headlines and content.
(setq org-startup-indented t)
(setq org-tags-column
      (- 3 fill-column)) ; leave room for 3 dots when folded

;; I use these commands too much outside of org-mode to not have them here, too.
(define-key org-mode-map (kbd "<C-S-return>") 'open-line-above)
(define-key org-mode-map (kbd "<C-S-down>") 'move-text-down)
(define-key org-mode-map (kbd "<C-S-up>") 'move-text-up)
(define-key org-mode-map (kbd "M-p") 'backward-paragraph)
(define-key org-mode-map (kbd "M-n") 'forward-paragraph)
(define-key org-mode-map (kbd "<M-return>") 'org-meta-return)

;; Move windows, even in org-mode
(define-key org-mode-map (kbd "<S-right>") 'windmove-right)
(define-key org-mode-map (kbd "<S-left>") 'windmove-left)
(define-key org-mode-map (kbd "<S-up>") 'windmove-up)
(define-key org-mode-map (kbd "<S-down>") 'windmove-down)

;; (define-key org-mode-map (kbd "<M-S-right>") 'buf-move-right)
;; (define-key org-mode-map (kbd "<M-S-left>") 'buf-move-left)
;; (define-key org-mode-map (kbd "<M-S-up>") 'buf-move-up)
;; (define-key org-mode-map (kbd "<M-S-down>") 'buf-move-down)

;; Stuff for reproducible research
;(add-to-list 'org-src-lang-modes ...)
;(setq org-src-preserve-indentation t)

;; Execute code blocks in these languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (emacs-lisp . t)
    (sh . t)
    ;(julia . t)
    (python . t)
    (ipython . t)
    ;(R . t)
    (clojure . t)
    ;(js . t)
    ;(matlab . t)
    ))

(setq org-babel-clojure-backend 'cider)

;; Line wrapping.
(visual-line-mode)
(visual-fill-column-mode)

;; Log to-do completion times with optional notes.
(setq org-log-done 'time)
(setq org-log-done 'note)

;; To-do dependencies
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
;; (setq org-agenda-dim-blocked-tasks 'invisible)

;; Agenda files
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files
      (directory-files org-directory t ".*\.org$"))
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 4)))

(setq org-refile-use-outline-path 'file) ; allow refiling as a top-level heading
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; Check out org-outline-path-complete-in-steps: https://stackoverflow.com/a/25089958

;; Don't ask me to confirm evaluation every time.
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-structure-template-alist
             '("R" "#+ATTR_REVEAL: " ""))
(setq org-html-inline-images t)

;; Mobile Org
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Styling
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Narrow to subtree when using org-agenda-follow-mode.
(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-subtree)))

(provide 'setup-org)
