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

;; org-mode to-do colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

;; Various colors
(setq theme-class '((class color) (min-colors 89)))
(custom-set-faces
 `(org-special-keyword ((,theme-class (:foreground "DimGray"))))
 `(org-property-value ((,theme-class (:foreground "Gray"))))
 `(link ((,theme-class (:foreground "SkyBlue3" :underline t))))
 `(org-date ((,theme-class (:foreground "SkyBlue3" :underline nil))))
 `(font-lock-comment-face ((,theme-class (:foreground "Chocolate1")))))

;; (use-package apropospriate
;;   :config
;;   (load-theme 'apropospriate-dark t))

;; Show bullets for lists (instead of dashes or asterisks).
(font-lock-add-keywords
 'org-mode '(("^ *\\([-]\\) "
              (0 (prog1 () (compose-region
                            (match-beginning 1)
                            (match-end 1)
                            "•"))))))

;; ;; Pick a nice proportional font.
;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@variable-tuple))))
;;    `(org-level-7 ((t (,@variable-tuple))))
;;    `(org-level-6 ((t (,@variable-tuple))))
;;    `(org-level-5 ((t (,@variable-tuple))))
;;    `(org-level-4 ((t (,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@variable-tuple :height 2.0 :underline nil))))))

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

;; Soft-wrap lines
;; (require 'visual-line-mode)
(global-visual-line-mode)
;; (require 'visual-fill-column-mode)
(global-visual-fill-column-mode)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

(provide 'appearance)
