
;; Styling of headline bullets in org-mode
(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Set up org-agenda
(use-package org-agenda-mode
  :defer t
  :ensure t

  :init
  ;; Agenda files and refiling targets
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files
        (directory-files org-directory t ".*\.org$"))
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 2)))

  ;; To-do dependencies
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)

  ;; Narrow to subtree when using org-agenda-follow-mode.
  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree))))

;; Set up org-mode
(use-package org-mode
  :defer t
  :ensure t

  :bind (("C-c c" . org-agenda)

         :map org-mode
         ("<C-S-return>" . open-line-above)
         ("<C-S-down>"   . move-text-down)
         ("<C-S-up>"     . move-text-up)
         ("M-p"          . backward-paragraph)
         ("M-n"          . forward-paragraph)
         ("<M-return>"   . org-meta-return)
         ;; windmove
         ("<S-right>"    . windmove-right)
         ("<S-left>"     . windmove-left)
         ("<S-up>"       . windmove-up)
         ("<S-down>"     . windmove-down)
         ;; ;; buf-move
         ;; ("<M-S-right>"  . buf-move-right)
         ;; ("<M-S-left>"   . buf-move-left)
         ;; ("<M-S-up>"     . buf-move-up)
         ;; ("<M-S-down>"   . buf-move-down))

  :config
  (progn
    ;; Indent headlines and content.
    (setq org-startup-indented t)
    (setq org-tags-column
          (- 3 fill-column)) ; leave room for 3 dots when folded

    ;; Enable line wrapping at word breaks at the set fill-column.
    (visual-line-mode nil)
    (visual-fill-column-mode t)

    ;; Don't ask me to confirm evaluation every time.
    (setq org-confirm-babel-evaluate nil)

    (add-to-list 'org-structure-template-alist
                 '("R" "#+ATTR_REVEAL: " ""))

    ;; Export settings
    (setq org-html-inline-images t)))
