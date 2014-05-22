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

;; I use this command too much outside of org-mode to not have it here, too.
(define-key org-mode-map (kbd "<C-S-return>") 'open-line-above)

(provide 'setup-org)
