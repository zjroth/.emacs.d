(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; -----------------------------------------------------------------------------
;; Themes I want to try out...

;; My modifications to this theme are in the theme file.
;; - Set the base00 dark variant to #3C3C3C (in the theme file).
;; - https://github.com/waymondo/apropospriate-theme
;; - org-mode colors: ~/.emacs.d/elpa/org-9.1.13/org-faces.el
(use-package apropospriate-theme
  :init (progn
          (defun color-match-lightness (of-color to-color)
            (color-set-lightness of-color
                                 (color-get-lightness to-color)))

          (defun color-get-lightness (color)
            (nth 2 (apply 'color-rgb-to-hsl
                          (color-name-to-rgb color))))

          (defun color-set-lightness (color lightness)
            (let ((color-hsl (apply 'color-rgb-to-hsl
                                    (color-name-to-rgb color))))
              (apply 'color-rgb-to-hex
                     (apply 'color-hsl-to-rgb
                            (list (nth 0 color-hsl)
                                  (nth 1 color-hsl)
                                  lightness))))))

  :config
  (progn
    (load-theme 'apropospriate-dark t)

    ;; Style the mode line.
    (use-package smart-mode-line
      :config (sml/setup))

    ;; In the definition of apropospriate-with-color-variables, I redefine the
    ;; color base00 as follows to get a darker background:
    ;;     (base00 (if (eq variant 'light) "#FAFAFA" "#323232"))
    (apropospriate-with-color-variables
      'dark

      ;; Mode line colors
      (custom-theme-set-faces
       'smart-mode-line-dark
       `(mode-line     ((t :foreground "gray60" :background ,base00-2 :inverse-video nil)))
       `(sml/modified  ((t :inherit sml/not-modified :foreground "#FF3333" :weight bold))))

      ;; All other colors
      (custom-theme-set-faces
       'apropospriate-dark
       `(default ((,class (:background ,base00 :foreground ,base03))))

       ;; `(font-lock-string-face ((,class (:foreground ,base02))))
       `(font-lock-string-face ((,class (:foreground ,green))))
       `(mode-line-inactive ((,class (:box (:line-width 4 :color ,base00+1 :style nil)
                                           :background ,base00-2 :foreground ,base02
                                           :height ,(or apropospriate-mode-line-height 1.0)))))

       ;; ivy and swiper
       `(ivy-minibuffer-match-face-1
         ((,class (:foreground nil :background nil :underline (:style line :color ,base02)))))
       `(ivy-minibuffer-match-face-2
         ((,class (:inherit ivy-minibuffer-match-face-1 :foreground ,yellow))))
       `(ivy-minibuffer-match-face-3
         ((,class (:inherit ivy-minibuffer-match-face-2 :foreground ,orange))))
       `(ivy-minibuffer-match-face-4
         ((,class (:inherit ivy-minibuffer-match-face-2 :foreground ,indigo-1))))

       `(swiper-match-face-1 ((,class (:inherit isearch-lazy-highlight-face))))
       `(swiper-match-face-2 ((,class (:inherit isearch))))
       `(swiper-match-face-3 ((,class (:inherit match))))
       `(swiper-match-face-4 ((,class (:inherit isearch-fail))))
       `(swiper-line-face ((,class (:inherit highlight))))

       ;; `(swiper-match-face-1 ((,class (:inherit ivy-minibuffer-match-face-1))))
       ;; `(swiper-match-face-2 ((,class (:inherit ivy-minibuffer-match-face-2))))
       ;; `(swiper-match-face-3 ((,class (:inherit ivy-minibuffer-match-face-3))))
       ;; `(swiper-match-face-4 ((,class (:inherit ivy-minibuffer-match-face-4))))
       ;; `(swiper-line-face ((,class (:background ,highlight-line-color))))

       ;; dired-mode
       `(dired-directory ((,class (:foreground ,purple))))
       `(dired-header ((,class (:foreground ,blue))))
       `(dired-ignored ((,class (:foreground ,base01))))
       `(dired-symlink ((,class (:foreground ,teal))))

       ;; org-mode
       `(org-priority ((,class (:foreground ,indigo-1))))
       `(org-special-keyword ((,class (:foreground ,base01))))
       `(org-block ((,class (:background ,base00+1))))
       `(org-block-begin-line ((,class (:inherit font-lock-comment-face :background ,base00-1))))
       `(org-block-end-line ((,class (:inherit font-lock-comment-face :background ,base00-1))))
       `(org-date ((,class (:foreground ,teal))))
       `(org-level-1 ((,class (:inherit header-line :foreground ,purple))))
       `(org-level-2 ((,class (:inherit header-line :foreground ,(color-darken-name purple 7)))))
       `(org-level-3 ((,class (:inherit header-line :foreground ,(color-darken-name purple 14)))))
       `(org-level-4 ((,class (:inherit header-line :foreground ,(color-darken-name purple 21)))))
       `(org-level-5 ((,class (:inherit header-line :foreground ,(color-darken-name purple 28)))))
       `(org-level-6 ((,class (:inherit header-line :foreground ,(color-darken-name purple 35)))))
       `(org-level-7 ((,class (:inherit header-line :foreground ,(color-darken-name purple 42)))))
       `(org-level-8 ((,class (:inherit header-line :foreground ,(color-darken-name purple 49)))))

       ;; org-agenda
       `(org-habit-alert-face ((,class (:background "DarkRed" :foreground "white"))))
       `(org-habit-ready-face ((,class (:background "ForestGreen" :foreground "white"))))
       `(org-habit-clear-future-face ((,class (:background ,base00+1))))
       `(org-agenda-dimmed-todo-face ((,class (:foreground ,base00+3))))
       `(org-agenda-clocking ((,class (:background ,light-emphasis))))
       `(org-agenda-current-time ((,class (:foreground ,red))))
       `(org-time-grid ((,class (:foreground ,base02))))
       `(org-agenda-date-weekend ((,class (:inherit org-agenda-date :foreground ,base01))))
       `(org-agenda-structure ((,class (:foreground ,purple :weight bold))))
       `(org-agenda-date-weekend ((,class (:foreground ,base00+3 :slant italic))))

       `(mode-line ((,class (:box (:line-width 4 :color ,light-emphasis :style nil)
                                  :background ,base00-2 :foreground ,base03
                                  :height ,(or apropospriate-mode-line-height 1.0)))))
       `(mode-line-inactive ((,class (:box (:line-width 4 :color ,base00+1 :style nil)
                                           :background ,base00+1 :foreground ,base02
                                           :height ,(or apropospriate-mode-line-height 1.0)))))

       ;; ;; magit
       ;; `(magit-diff-context ((,class (:foreground ,base01 :background unspecified))))
       ;; `(magit-diff-context-highlight ((,class (:foreground ,base01 :background ,base00-1))))
       ;; `(magit-diff-hunk-heading ((,class (:foreground ,base00 :background ,base02))))
       ;; `(magit-diff-lines-heading ((,class (:background unspecified))))
       ;; `(magit-diff-hunk-heading-highlight ((,class (:inherit magit-diff-hunk-heading :background ,base03))))
       ;; `(magit-diff-our-highlight ((,class (:background unspecified))))
       ;; `(magit-diff-removed ((,class (:foreground ,red :background unspecified))))
       ;; `(magit-diff-removed-highlight ((,class (:foreground (color-darken-name "#E57373" 40) :background ,base00-1))))
       ;; `(magit-diff-added-highlight ((,class (:foreground (color-darken-name "#C5E1A5" 15) :background ,base00-1))))
       `(magit-diff-removed-highlight ((,class (:foreground "#723939" :background ,base00-1))))
       `(magit-diff-added-highlight ((,class (:foreground "#647253" :background ,base00-1))))
       `(diff-refine-removed ((,class (:foreground ,red :weight bold))))
       `(diff-refine-added ((,class (:foreground ,green :weight bold))))

       ;; `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
       ;; `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
       ;; `(ediff-odd-diff-A ((,class ( :foreground ,base03 :background nil :inverse-video t))))
       ;; `(ediff-odd-diff-B ((,class ( :foreground ,base03 :background nil :inverse-video t))))

       `(ediff-current-diff-A ((,class (:foreground ,base00 :background ,red-1))))
       ;; `(ediff-current-diff-Ancestor ((,class (:foreground ,base03 :background ,red-1))))
       `(ediff-current-diff-B ((,class (:foreground ,base00 :background ,green))))
       ;; `(ediff-current-diff-C ((,class (:foreground ,base03 :background ,blue-1))))
       ;; `(ediff-even-diff-A ((,class (:background ,base00+2))))
       ;; `(ediff-even-diff-Ancestor ((,class (:background ,base00+2))))
       ;; `(ediff-even-diff-B ((,class (:background ,base00+2))))
       ;; `(ediff-even-diff-C ((,class (:background ,base00+2))))
       `(ediff-fine-diff-A ((,class (:foreground ,base00 :background ,red))))
       ;; `(ediff-fine-diff-Ancestor ((,class (:foreground ,base03 :background ,red-1))))
       `(ediff-fine-diff-B ((,class (:foreground ,base00 :background ,green-1))))
       ;; `(ediff-fine-diff-C ((,class (:foreground ,base03 :background ,blue-1 ))))
       ;; `(ediff-odd-diff-A ((,class (:background ,base01))))
       ;; `(ediff-odd-diff-Ancestor ((,class (:background ,base01))))
       ;; `(ediff-odd-diff-B ((,class (:background ,base01))))
       ;; `(ediff-odd-diff-C ((,class (:background ,base01))))

       `(region ((,class (:background ,base02))))
       ))
    ))

;; (use-package zenburn-theme
;;   :config
;;   ;; Flip the default "normal" background color and highlighted background color.
;;   (setq zenburn-override-colors-alist
;;         '(("zenburn-bg-1"     . "#3F3F3F")
;;           ("zenburn-bg"       . "#2B2B2B")))
;;
;;   ;; (zenburn-with-color-variables
;;   ;;   (custom-theme-set-variables
;;   ;;    `(org-block ((,class (:background ,zenburn-bg+05 :foreground ,zenburn-bg+3))))
;;   ;;    `(org-block-begin-line ((,class (:background ,zenburn-bg-2 :foreground ,zenburn-bg+2))))
;;   ;;    `(org-block-end-line ((,class (:background ,zenburn-bg-2 :foreground ,zenburn-bg+2))))
;;   ;;    `(org-quote ((,class (:inherit org-block :slant italic))))
;;   ;;    `(org-verse ((,class (:inherit org-block :slant italic))))
;;   ;;    ))
;;   )

;; (use-package badger-theme
;;   :config (load-theme 'badger t))

;; ;; Too bright.  Org headlines have been resized.  Nice mode line.
;; (use-package monokai-theme
;;   :config (load-theme 'monokai t))

;; ;; Not bad.
;; (use-package atom-one-dark-theme
;;   :config (load-theme 'atom-one-dark t))

;; ;; Org headlines have been resized.  Hrumph...
;; (require 'spacemacs-dark-theme)
;; (load-theme 'spacemacs-dark t)

;; ;; Some good ideas, but it just seems like too much.
;; (use-package material-theme
;;   :config (load-theme 'material t))

;; -----------------------------------------------------------------------------

;; ;; Set the default font and background colors.
;; (set-face-background 'default "#1B1B1B")
;; (set-face-foreground 'default "#EEEEEE")
;;
;; ;; Set the color for a selected region.
;; (set-face-background 'region "#464740")
;;
;; ;; Highlight the current line. Set the color of that highlighting.
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#333333")
;;
;; ;; Highlight in yasnippet
;; ;; (set-face-background 'yas/field-highlight-face "#333399") ;; TODO: This is not working.  Why?
;; (set-face-foreground 'font-lock-warning-face "#ff6666")

;; -----------------------------------------------------------------------------

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Soft-wrap lines
(global-visual-line-mode nil)
(use-package visual-fill-column
  :demand t
  ;; :hook (visual-line-mode . visual-fill-column-mode)
  :config
  (progn
    ;; The default function for window splitting (`split-window-sensibly`) does
    ;; not play well with visual-fill-column mode.  I think that this is because
    ;; the mode changes the return value of `window-width`.  This function makes
    ;; window splitting sensible again.  Huzzah!
    (setq split-window-preferred-function
          #'visual-fill-column-split-window-sensibly)

    ;; Enable this everywhere.
    (global-visual-fill-column-mode nil)
    ))

;; Indent to a reasonable level when wrapping lines.
(use-package adaptive-wrap
  :pin gnu)

(provide 'appearance)
