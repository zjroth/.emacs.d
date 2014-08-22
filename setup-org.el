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

;; I use these commands too much outside of org-mode to not have them here, too.
(define-key org-mode-map (kbd "<C-S-return>") 'open-line-above)
(define-key org-mode-map (kbd "<C-S-down>") 'move-text-down)
(define-key org-mode-map (kbd "<C-S-up>") 'move-text-up)

;; Stuff for reproducible research
;(add-to-list 'org-src-lang-modes ...)
;(setq org-src-preserve-indentation t)

(provide 'setup-org)
