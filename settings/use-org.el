;; Style the headline bullets in org-mode
(use-package org-bullets
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Functions for creating journal entries.  Might be useful.
(use-package org-journal
  :init
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-file-type 'weekly)
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
  :defer t

  :config
  (setq org-doing-file "~/Documents/doing.org"))

;; Try to keep me on track.
(use-package org-pomodoro
  :defer t)

;; Interaction with JIRA from org-mode.
(use-package org-jira
  :defer t

  :init
  (setq jiralib-url "https://jira.mutualofomaha.com/jiradc"))

;; Create mermaid.js diagrams.
(use-package ob-mermaid
  :defer t
  :init
  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))

;; org-mode
(use-package org
  :pin gnu
  :defer t
  :after ivy

  :init
  (progn
    ;; (require 'setup-org-protocol)
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

    ;; Get the old easy-template behavior (e.g., expanding "<s" to a source-code block).
    (add-to-list 'org-modules 'org-tempo t)

    ;; Be explicit(-ish) about how org-mode should open files using external
    ;; programs.  This affects, for instance, how an exported PDF is opened.
    (setq org-file-apps '((auto-mode . emacs)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.pdf\\'" . default)))

    ;; Archiving
    (setq org-archive-location "archive/datetree.org::datetree/")

    ;; org-id stuff
    ;; Don't forget about this (related to org-id): https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode#16247032
    ;; ...or, similarly, this: https://writequit.org/articles/emacs-org-mode-generate-ids.html
    (require 'org-id)
    (setq org-id-link-to-org-use-id t)

    ;; (defun zjr/org-block-task (&optional id)
    ;;   (interactive (list (ivy-read "Select blocking task: "
    ;;                                (list "dev" "prod")
    ;;                                :require-match t)))
    ;;   (let ((curr-blockers (org-entry-get (point) "BLOCKER"))
    ;;         (new-blocker (or id
    ;;                          ()))
    ;;     (org-set-property "BLOCKER"
    ;;                       (if curr-blockers
    ;;                           (concat curr-blockers " " new-blocker)
    ;;                         new-blocker)))))

    (defun zjr/list-org-agenda-headlines ()
      "Return a list of completion candidates for `counsel-org-agenda-headlines'."
      (-non-nil
       (org-map-entries (lambda ()
                          (if (nth 2 (org-heading-components))
                              (list (org-display-outline-path 'filename 'current
                                                              " ▶ " 'just-return-string)
                                    buffer-file-name
                                    (point))))
                        "-todo=\"DONE\"-todo=\"CANCELED\"-todo=\"FINISHED\""
                        'agenda
                        'archive 'comment)))

    (defun zjr/org-block-task ()
      (interactive)
      (ivy-read "Select blocking task: "
                (zjr/list-org-agenda-headlines)
                :require-match t
                :action (lambda (match)
                          (message (nth 1 id))))
      ;; (let ((curr-blockers (org-entry-get (point) "BLOCKER"))
      ;;       (new-blocker (or id
      ;;                        ()))
      ;;   (org-set-property "BLOCKER"
      ;;                     (if curr-blockers
      ;;                         (concat curr-blockers " " new-blocker)
      ;;                       new-blocker))))
      )

    (setq counsel-outline-path-separator " ▶ ")
    (setq counsel-org-headline-display-todo nil)
    ;; (car (counsel-org-agenda-headlines--candidates))
    ;; (org-display-outline-path
    ;;  'filename 'current " ▶ " 'just-return-string)

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
    (setq org-log-into-drawer t)
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

    ;; ;; Create more "easy templates" that I use.
    ;; (add-to-list 'org-structure-template-alist
    ;;              '("N" "#+NAME: "))
    (setq org-structure-template-alist
          (remove '("N" "#+NAME: ") org-structure-template-alist))

    ;; Effort
    (add-to-list 'org-global-properties
                 '("Effort_ALL" . "0 0:10 0:30 1:00 2:00 4:00 8:00"))

    ;; To-do states
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@)")
            ;;("TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
            (sequence "WAIT(w@)" "MAYBE(m)" "HOLD(h@)" "|" "CANCELED(c@)")
            ;; (sequence "THOMAS" "EMILY" "BRANT" "BRIAN" "|" "DONE")
            (sequence "READ(r)" "READING(e)" "|" "FINISHED(f)")))
    (setq org-use-fast-todo-selection t)
    (setq org-todo-keywords-for-agenda org-todo-keywords)

    ;; To-do label colors
    (when (fboundp 'apropospriate-with-color-variables)
      (apropospriate-with-color-variables
        'dark
        (setq org-todo-keyword-faces
              `(("TODO" . (:foreground ,base00+3))
                ("NEXT" . (:foreground ,yellow-1))
                ;;("ACTIVE" . (:foreground ,green :weight bold))
                ("STARTED" . (:foreground ,orange))

                ("WAIT" . (:foreground ,orange))
                ("MAYBE" . (:foreground ,base00+3))
                ("HOLD" . (:foreground ,orange))

                ;; ("READ" . (:foreground ,(color-match-lightness brown base00+3)))
                ;; ("READING" . (:foreground ,(color-match-lightness teal green) :weight bold))
                ;; ("FINISHED" . (:foreground ,light-emphasis :strike-through t))
                ("READ" . (:foreground ,(color-set-lightness brown 0.31)))
                ("READING" . (:foreground ,(color-set-lightness brown 0.5) :weight bold))
                ("FINISHED" . (:foreground ,light-emphasis :strike-through t))
                ;; ("FINISHED" . (:foreground ,(color-set-lightness brown 0.15) :strike-through t))

                ("DONE" . (:foreground ,light-emphasis :strike-through t))
                ("CANCELED" . (:foreground ,light-emphasis :strike-through t)))))))

  :bind (("C-c c" . org-capture)
         ("C-c C-a" . org-agenda)
         ;; Link-related.  NOTE: (global-unset-key (kbd "C-c l"))
         ("C-c l i" . org-insert-link)
         ("C-c l l" . org-insert-link)
         ("C-c l s" . org-store-link)
         ("C-c l c" . org-copy-link-at-point)
         ;; Jump to an org entry.
         ("C-c C-j"   . counsel-org-goto-all)) ; intentionally bound twice

  :bind (:map org-mode-map
              ("<C-S-return>" . open-line-above)
              ("<C-S-down>"   . move-text-down)
              ("<C-S-up>"     . move-text-up)
              ("M-p"          . backward-paragraph)
              ("M-n"          . forward-paragraph)
              ("<M-return>"   . org-meta-return)
              ("<C-M-return>" . org-insert-todo-heading-respect-content)
              ;; Jump to an org entry.
              ("C-c C-j"      . counsel-org-goto-all) ; intentionally bound twice

              ;; ivy
              ;; ("C-h a"     . helm-apropos)
              ;; ("C-x b"     . helm-buffers-list)
              ;; ("C-x c o"   . helm-occur)
              ;; ("C-x c SPC" . helm-all-mark-rings)
              )

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
          ;; (- 3 visual-fill-column-width)
          72 ; adjust for zoomed, half-screen display at Crescent
          )  ; leave room for 3 dots when folded

    ;; Make lists look like use bullets (in place of the actual hyphen (or
    ;; asterisk) that's really there.
    (font-lock-add-keywords
     'org-mode '(("^ *\\([-]\\) "
                  (0 (prog1 () (compose-region (match-beginning 1)
                                               (match-end 1)
                                               "•"))))))

    ;; Always separate entries by a blank line.
    (setq org-blank-before-new-entry
          '((heading . t)
            (plain-list-item . auto)))

    ;; Fontify org-mode code blocks
    (setq org-src-fontify-natively t)

    ;; Don't ask me to confirm evaluation every time.
    (setq org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '( (clojure . t)
        (emacs-lisp . t)
        ;; (ipython . t)
        ;; (js . t)
        (julia . t)
        ;; (matlab . t)
        (python . t)
        ;; (R . t)
        (shell . t)
        ;; (sql . t)
        ))

    ;; Show only hours and minutes in time durations.  (Days are confusing:
    ;; They're 24-hour days in a setting of 8-hour work days.)
    (setq org-duration-format 'h:mm)

    (require 'setup-org-punch-in)

    ;; Export settings
    (require 'setup-org-export)
    ))

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "Documents/org/org-roam/" home-dir))
  (org-roam-dailies-directory "daily/")
  ;; (org-roam-capture-templates '(("d" "default" plain "%?"
  ;;                                :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                                                   "#+title: ${title}\n\n")
  ;;                                :unnarrowed t)
  ;;                               ("i" "Crescent issue" plain "%?"
  ;;                                :target (file+head "%<%Y%m%d%H%M%S>-linear-issue-${slug}.org"
  ;;                                                   "#+title: Linear Issue ${title}\n#+url: https://linear.app/crescent-financial-inc/issue/${title}\n\n\nDescription:\n- Linear title :: \n- Goal :: \n- Motivation :: \n- Outcome :: \n\nNotes:\n- ")
  ;;                                :unnarrowed t)))
  :bind (("M-m b t" . org-roam-buffer-toggle)
         ("M-m M-m" . org-roam-node-find)
         ("M-m d" . org-roam-dailies-goto-today)
         ("M-m i" . org-roam-node-insert)
         ("M-m e" . org-roam-extract-subtree)))
(use-package org-roam-timestamps)
(use-package org-roam-ui)
(use-package org-transclusion)

(use-package org-modern
  :defer t)
;; (use-package olivetti
;;   :defer t)
(use-package writeroom-mode
  :defer t)
;; (use-package org-hyperscheduler
;;   :defer t)

(require 'setup-org-agenda)

(provide 'use-org)
