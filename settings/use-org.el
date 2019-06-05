
;; Style the headline bullets in org-mode
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Functions for creating journal entries.  Might be useful.
(use-package org-journal
  :init
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-file-format "%Y-%m-%d")
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min))))

;; ;; Similar to org-depend.el
;; (use-package org-edna
;;   :pin gnu
;;   :config
;;   (org-edna-load))

;; Track what I'm doing.
(use-package org-doing
  :config
  (setq org-doing-file "~/Documents/doing.org"))

;; Try to keep me on track.
(use-package org-pomodoro)

;; org-mode
(use-package org
  :pin "gnu"
  :defer t

  :init
  (progn
    (require 'setup-org-agenda)
    (require 'setup-org-protocol)
    (require 'setup-org-capture)

    ;; Set up (non-local) to-do dependencies.
    (load-file "~/.emacs.d/other-packages/org-depend.el")
    (require 'org-depend)

    ;; Track habits
    (add-to-list 'org-modules 'org-habit t)
    (setq org-habit-graph-column 81)
    (setq org-habit-preceding-days 21)
    (setq org-habit-following-days 1)
    (setq org-habit-show-habits-only-for-today t)
    (setq org-habit-today-glyph ?♡)
    (setq org-habit-completed-glyph ?✓)

    ;; Archiving
    (setq org-archive-location "archive/datetree.org::datetree/")

    ;; org-id stuff
    ;; Don't forget about this (related to org-id): https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode#16247032
    ;; ...or, similarly, this: https://writequit.org/articles/emacs-org-mode-generate-ids.html
    (require 'org-id)
    (setq org-id-link-to-org-use-id t)

    (defun org-set-property-if-missing (prop value)
      "Set a property on the entry if the property does not already exist."
      (interactive)
      (unless (org-entry-get (point) prop nil)
        (org-set-property prop value)))

    (defvar org-created-property-name "CREATED"
      "The name of the org-mode property that stores the creation date of the entry")
    (defun org-set-created-property ()
      (interactive)
      (org-set-property-if-missing org-created-property-name
                                   (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (defvar org-started-property-name "STARTED"
      "The name of the org-mode property that stores the date the entry was started")
    (defun org-set-started-property ()
      (interactive)
      (org-set-property-if-missing org-started-property-name
                                   (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (setq org-enforce-todo-dependencies t)
    (setq org-log-done 'time)
    (setq org-catch-invisible-edits t)
    (setq org-use-property-inheritance t)

    ;; Change how org-mode displays basic mark-up (bold, italic, etc.).
    (setq org-hide-emphasis-markers nil)
    (setq org-highlight-latex-and-related '(latex))

    ;; Preview LaTeX fragments
    (setq org-preview-latex-default-process 'dvisvgm)

    ;; Refiling
    (setq org-refile-allow-creating-parent-nodes t)
    (setq org-refile-use-outline-path 'file)
    ;(setq org-goto-interface 'outline)
    (setq org-outline-path-complete-in-steps nil)

    ;; A function for copying th org-mode link at the current point, which is
    ;; something that I often find myself doing.
    (defun org-copy-link-at-point ()
      ;; The code here was mostly taken from `org-insert-link`.
      "Copy the org-mode link at the given point (if it exists)."
      (interactive)
      (let ((at-link? (org-in-regexp org-bracket-link-regexp 1)))
        (cond (at-link? (kill-new (org-link-unescape
                                   (match-string-no-properties 1))))
              (t        (warn "Not currently at a link.")))))

    ;; Create more "easy templates" that I use.
    (add-to-list 'org-structure-template-alist
                 '("N" "#+NAME: "))

    ;; To-do states
    (setq org-todo-keywords
          '(("TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
            ("WAIT(w)" "MAYBE(m)" "|" "CANCELED(c)")
            ("READ(r)" "READING(e)" "|" "FINISHED(f)")))

    ;; To-do label colors
    (apropospriate-with-color-variables
      'dark
      (setq org-todo-keyword-faces
            `(("TODO" . (:foreground ,base00+3))
              ("NEXT" . (:foreground ,yellow-1))
              ("ACTIVE" . (:foreground ,green :weight bold))
              ("STARTED" . (:foreground ,orange))

              ("WAIT" . (:foreground ,orange))
              ("MAYBE" . (:foreground ,base00+3))

              ;; ("READ" . (:foreground ,(color-match-lightness brown base00+3)))
              ;; ("READING" . (:foreground ,(color-match-lightness teal green) :weight bold))
              ;; ("FINISHED" . (:foreground ,light-emphasis :strike-through t))
              ("READ" . (:foreground ,(color-set-lightness brown 0.31)))
              ("READING" . (:foreground ,(color-set-lightness brown 0.5) :weight bold))
              ("FINISHED" . (:foreground ,light-emphasis :strike-through t))
              ;; ("FINISHED" . (:foreground ,(color-set-lightness brown 0.15) :strike-through t))

              ("DONE" . (:foreground ,light-emphasis :strike-through t))
              ("CANCELED" . (:foreground ,light-emphasis :strike-through t)))))

    )

  ;(global-unset-key (kbd "C-c l"))
  :bind (("C-c c" . org-capture)
         ("C-c C-a" . org-agenda)
         ("C-c l i" . org-insert-link)
         ("C-c l l" . org-insert-link)
         ("C-c l s" . org-store-link)
         ("C-c l c" . org-copy-link-at-point)

         :map org-mode-map
         ("<C-S-return>" . open-line-above)
         ("<C-S-down>"   . move-text-down)
         ("<C-S-up>"     . move-text-up)
         ("M-p"          . backward-paragraph)
         ("M-n"          . forward-paragraph)
         ("<M-return>"   . org-meta-return))

  :hook ((org-mode                    . visual-fill-column-mode)
         (org-mode                    . org-display-inline-images)

         ;(remove-hook 'org-insert-heading-hook #'org-set-created-property)
         (org-insert-heading          . org-set-created-property)
         (org-capture-before-finalize . org-set-created-property)
         (org-clock-in                . org-set-started-property))

  :config
  (progn
    ;; Indent headlines and content.
    (setq org-startup-indented t)
    (setq org-tags-column
          (- 3 fill-column)) ; leave room for 3 dots when folded

    ;; Make lists look like use bullets (in place of the actual hyphen (or
    ;; asterisk) that's really there.
    (font-lock-add-keywords
     'org-mode '(("^ *\\([-]\\) "
                  (0 (prog1 () (compose-region (match-beginning 1)
                                               (match-end 1)
                                               "•"))))))

    ;; ;; Always separate entries by a blank line.
    ;; (setq org-blank-before-new-entry
    ;;       '((heading . t)
    ;;         (plain-list-item . auto)))

    ;; Fontify org-mode code blocks
    (setq org-src-fontify-natively t)

    ;; Don't ask me to confirm evaluation every time.
    (setq org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '( (emacs-lisp . t)
        (shell . t)
        ;; (julia . t)
        (python . t)
        ;; (ipython . t)
        ;; (R . t)
        ;; (clojure . t)
        ;; (js . t)
        ;; (matlab . t)
        ))

    ;; Show only hours and minutes in time durations.  (Days are confusing:
    ;; They're 24-hour days in a setting of 8-hour work days.)
    (setq org-duration-format 'h:mm)

    ;; Punch in/out for time tracking.  This is lightly modified from
    ;; http://doc.norang.ca/org-mode.html#Clocking.
    (progn
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)

      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)

      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)

      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)

      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)

      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)

      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)

      (setq bh/keep-clock-running nil)

      (defun bh/is-task-p ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      (defun bh/is-project-p ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun bh/clock-in-to-next (kw)
        "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
        (when (not (and (boundp 'org-capture-mode) org-capture-mode))
          (cond
           ((and (member (org-get-todo-state) (list "TODO"))
                 (bh/is-task-p))
            "NEXT")
           ((and (member (org-get-todo-state) (list "NEXT"))
                 (bh/is-project-p))
            "TODO"))))

      (defun bh/find-project-task ()
        "Move point to the parent (project) task if any"
        (save-restriction
          (widen)
          (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
            (while (org-up-heading-safe)
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq parent-task (point))))
            (goto-char parent-task)
            parent-task)))

      (defun zjr/punch-in (arg)
        "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
        (interactive "p")
        (setq bh/keep-clock-running t)
        (if (equal major-mode 'org-agenda-mode)
            ;;
            ;; We're in the agenda
            ;;
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-point-at marker (org-get-tags-at))))
              (if (and (eq arg 4) tags)
                  (org-agenda-clock-in '(16))
                (zjr/punch-in-clock-default-task)))
          ;;
          ;; We are not in the agenda
          ;;
          (save-restriction
            (widen)
                                        ; Find the tags on the current task
            (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                (org-clock-in '(16))
              (zjr/punch-in-clock-default-task)))))

      (defun zjr/punch-out ()
        (interactive)
        (setq bh/keep-clock-running nil)
        (when (org-clock-is-active)
          (org-clock-out))
        (org-agenda-remove-restriction-lock))

      (defun bh/clock-in-default-task ()
        (save-excursion
          (org-with-point-at org-clock-default-task
            (org-clock-in))))

      (defun bh/clock-in-parent-task ()
        "Move point to the parent (project) task if any and clock in"
        (let ((parent-task))
          (save-excursion
            (save-restriction
              (widen)
              (while (and (not parent-task) (org-up-heading-safe))
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (if parent-task
                  (org-with-point-at parent-task
                    (org-clock-in))
                (when bh/keep-clock-running
                  (bh/clock-in-default-task)))))))

      (defvar zjr/punch-in-default-id "801F3BD3-D2E8-4C4A-82E7-5C74EA515658")

      (defun zjr/punch-in-clock-default-task ()
        (interactive)
        (setq bh/keep-clock-running t)
        (org-with-point-at (org-id-find zjr/punch-in-default-id 'marker)
          (org-clock-in '(16))))

      (defun bh/clock-out-maybe ()
        (when (and bh/keep-clock-running
                   (not org-clock-clocking-in)
                   (marker-buffer org-clock-default-task)
                   (not org-clock-resolving-clocks-due-to-idleness))
          (bh/clock-in-parent-task)))

      (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append))

    ;; Export settings
    (use-package htmlize)
    (setq org-html-inline-images t)

    (require 'ox-org)

    (require 'ox-latex)
    (setq org-latex-listings 'minted)
    (setq org-export-with-smart-quotes t)
    (add-to-list 'org-latex-packages-alist
                 '("newfloat" "minted"))
    (setq org-latex-pdf-process
          '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;;
    ;; ;; Display inline images
    ;; (add-hook 'org-mode-hook 'org-display-inline-images)

    ;; ;; Allow exporting to jupyter notebooks.
    ;; (load-file "~/.emacs.d/other-packages/ox-ipynb/ox-ipynb.el")
    ;; (require 'ox-ipynb)

    (use-package ox-gfm)

    ))

(provide 'use-org)
