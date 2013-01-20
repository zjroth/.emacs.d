(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Set the default font and background colors.
(set-face-background 'default "#1B1B1B")
(set-face-foreground 'default "#EEEEEE")

;; Set the color for a selected region.
(set-face-background 'region "#464740")

;; Highlight the current line. Set the color of that highlighting.
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
 
;; Highlight in yasnippet
;; (set-face-background 'yas/field-highlight-face "#333399") ;; TODO: This is not working.  Why?
(set-face-foreground 'font-lock-warning-face "#ff6666")

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (turn-off-tool-bar)
  (tooltip-mode -1)
  (turn-off-tool-bar)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

(provide 'appearance)
