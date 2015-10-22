;; ;; Subtler highlight
;; (set-face-background 'magit-item-highlight "#121212")
;; (set-face-foreground 'diff-context "#666666")
;; (set-face-foreground 'diff-added "#00cc33")
;; (set-face-foreground 'diff-removed "#ff0000")
;;
;; ;; Load git configurations
;; ;; For instance, to run magit-svn-mode in a project, do:
;; ;;
;; ;;     git config --add magit.extension svn
;; ;;
;; (add-hook 'magit-mode-hook 'magit-load-config-extensions)
;;
;; ;; C-x C-k to kill file on line
;;
;; (defun magit-kill-file-on-line ()
;;   "Show file on current magit line and prompt for deletion."
;;   (interactive)
;;   (magit-visit-item)
;;   (delete-current-buffer-file)
;;   (magit-refresh))
;;
;; (define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

;; full screen magit-status

(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

;; don't prompt me

(set-default 'magit-unstage-all-confirm nil)
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-revert-buffers 'silent)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; (require 'magit-commit-training-wheels)
;; (ad-activate 'magit-log-edit-commit)

(provide 'setup-magit)
