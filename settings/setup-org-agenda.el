(use-package org-agenda
  :ensure org
  :pin "gnu"

  :init
  (progn
    ;; Agenda files and refiling targets
    (setq org-directory "~/Documents/org")
    (setq org-agenda-files
          (directory-files org-directory t ".*\.org$"))
    (setq org-refile-targets
          '((org-agenda-files :maxlevel . 100)))

    ;; Toggle visibility of blocked entries.
    (defun org-agenda-cycle-blocked-task-visibility ()
      (interactive)
      (setq org-agenda-dim-blocked-tasks
            (cond ((eq org-agenda-dim-blocked-tasks 'invisible)         t)
                  (t 'invisible)))
      (org-agenda-redo)
      (message "Blocked tasks %s..."
               (cond ((eq org-agenda-dim-blocked-tasks t)           "dimmed")
                     ((eq org-agenda-dim-blocked-tasks 'invisible)  "hidden"))))


  :bind (:map org-agenda-mode-map
              ("V" . org-agenda-cycle-blocked-task-visibility))

  :config
  (progn
    ;; -----------------------------------------------------------------------------
    ;; Miscellaneous
    ;; -----------------------------------------------------------------------------

    ;; Hanging indent in the agenda buffer.
    (defun set-agenda-adaptive-wrap-extra-indent ()
      (setq adaptive-wrap-extra-indent 12))
    (add-hook 'org-agenda-mode-hook 'adaptive-wrap-prefix-mode)
    (add-hook 'org-agenda-mode-hook 'set-agenda-adaptive-wrap-extra-indent)

    ;; (defcustom org-agenda-prefix-format
    ;;   '((agenda  . " %i %-12:c%?-12t% s")
    ;;     (todo  . " %i %-12:c")
    ;;     (tags  . " %i %-12:c")
    ;;     (search . " %i %-12:c")))

    ;; Only show closed items in logbook mode.
    (setq org-agenda-log-mode-items '(closed))

    ;; What to do with to-do items that have a deadline or that been scheduled.
    ;; Note that this does not affect the agenda, just to-do lists that have been
    ;; generated.  The idea for using `'all` is that I am only looking through my
    ;; global to-do list if I'm looking for things that haven't already been
    ;; scheduled.
    (setq org-agenda-todo-ignore-scheduled 'all)
    (setq org-agenda-todo-ignore-deadlines 'far)

    ;; To-do dependencies
    (setq org-agenda-dim-blocked-tasks t)
    ;; (setq org-agenda-dim-blocked-tasks 'invisible)

    ;; Narrow to subtree when using org-agenda-follow-mode.
    (advice-add 'org-agenda-goto :after
                (lambda (&rest args)
                  (org-narrow-to-subtree)))

    ;; Display the path in the minibuffer when "hovering" over an item.
    (defun org-agenda-do-context-action ()
      "Show outline path and, maybe, follow mode window."
      (let ((m (org-get-at-bol 'org-marker)))
        (when (and (markerp m) (marker-buffer m))
          (and org-agenda-follow-mode
               (if org-agenda-follow-indirect
                   (org-agenda-tree-to-indirect-buffer nil)
                 (org-agenda-show)))
          (and org-agenda-show-outline-path
               (org-with-point-at m (org-display-outline-path
                                     'filename 'current " ▶ "))))))

    ;; -----------------------------------------------------------------------------
    ;; Customize the agenda view
    ;; -----------------------------------------------------------------------------

    ;; Start the agenda on Sunday.
    (setq org-agenda-start-on-weekday 0)

    ;; Do not show archived items.
    (setq org-agenda-archives-mode 'trees)

    ;; The default view that I use (a "block agenda").
    ;; - https://orgmode.org/manual/Agenda-views.html
    ;; - https://orgmode.org/manual/Block-agenda.html#Block-agenda
    ;; - https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    ;; - https://stackoverflow.com/a/31653151
    (setq org-agenda-custom-commands
          '(("d" "Today's agenda and all TODO items"
             (
              ;; (tags "+SCHEDULED=\"<today>\"")
              ;; (todo "READING")
              ;; (todo "ACTIVE|NEXT|WAIT")
              (agenda "" ((org-agenda-span 1)
                          (org-agenda-overriding-header "Scheduled items...")))  ; show today's agenda
              (todo "WAIT"
                    ((org-agenda-overriding-header "Waiting...")))
              (todo ""
                    ((org-agenda-overriding-header "Unscheduled items...")))
              ;; (todo "TODO|READ|STARTED|NEXT|ACTIVE")
              ;; (tags-todo "-TODO=\"NEXT\"-TODO=\"WAIT\"")       ; all to-do states except NEXT & WAIT
              ;; (alltodo "")                      ; show all to-do items in any state
              ;; (stuck "")                        ; show all stuck projects
              ))
            ("f" "Items closed within the past week"
             ((tags "+CLOSED>=\"<-7d>\"")))
            ("." "Items tagged with \"today\""
             ((tags "today")))
            ))

    ;; Set the symbol used to separate different agenda blocks.
    (setq org-agenda-block-separator ?ׄ)    ; ?= ; 32

    ;; ;; Sort agenda items.
    ;; ;; - Consider modifying the variable `org-agenda-cmp-user-defined`.
    ;; (defun my/org-agenda-cmp (a b)
    ;;   (message "a = ............................................................")
    ;;   (message a)
    ;;   (message "b = ............................................................")
    ;;   (message b)
    ;;   1
    ;;   )
    ;; (setq org-agenda-cmp-user-defined
    ;;       #'my/org-agenda-cmp)
    ;; (setq org-agenda-sorting-strategy
    ;;       '((agenda habit-down time-up priority-down category-keep)
    ;;         (todo user-defined-up priority-down category-keep tsia-up)
    ;;         (tags priority-down category-keep)
    ;;         (search category-keep)))
    ;; ;; (setq org-agenda-sorting-strategy
    ;; ;;       '((agenda habit-down time-up priority-down category-keep)
    ;; ;;         (todo priority-down category-keep tsia-up)
    ;; ;;         (tags priority-down category-keep)
    ;; ;;         (search category-keep)))

    ;; -----------------------------------------------------------------------------
    ;; Time grid (in the agenda view)
    ;; -----------------------------------------------------------------------------

    ;; ;; TODO: Attempt to change the color of time-grid lines and of weekend headings.
    ;; (apropospriate-with-color-variables
    ;;   'dark
    ;;   (setq org-agenda-date-weekend '(:inherit org-agenda-date :foreground ,base01 :weight bold)
    ;;         org-agenda-calendar-event '(:inherit default :foreground ,base01)))

    ;; Format of the time grid lines (hours) and the current time.
    (setq org-agenda-time-grid
          `((daily today remove-match)
            (0600 0700 0800 0900 1000 1100 1200 1300 1400 1500)
            " ⟵——————⟶" "                "))
    ;; " ————————" "                "))
    ;; "  →      " "                "))

    ;; How the current time is displayed.
    (setq org-agenda-current-time-string
          "----------------------NOW----------------------")

    ;; I still think in AM/PM.
    (setq org-agenda-timegrid-use-ampm t)

    ;; Redefine this function to get AM/PM (and leading spaces) instead of am/pm.
    (defun org-agenda-time-of-day-to-ampm (time)
      "Convert TIME of a string like \"13:45\" to an AM/PM style time string."
      (let* ((hour-number (string-to-number (substring time 0 -3)))
             (minute (substring time -2))
             (ampm " AM"))
        (cond
         ((equal hour-number 12)
          (setq ampm " PM"))
         ((> hour-number 12)
          (setq ampm " PM")
          (setq hour-number (- hour-number 12))))
        (concat
         (if org-agenda-time-leading-zero
             (format "%02d" hour-number)
           (format "%02s" (number-to-string hour-number)))
         ":" minute ampm)))
    ))

;; -----------------------------------------------------------------------------
;; Allow loading this from elsewhere.
;; -----------------------------------------------------------------------------

(provide 'setup-org-agenda)
