(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(elfeed-feeds (quote ("https://www.mathjobs.org/jobs?joblist-0-----rss")))
 '(ido-use-filename-at-point nil)
 '(org-agenda-files
   (quote
    ("/home/zjr/Documents/org/events.org" "/home/zjr/Documents/org/inbox.org" "/home/zjr/Documents/org/learning.org" "/home/zjr/Documents/org/lists.org" "/home/zjr/Documents/org/misc.org" "/home/zjr/Documents/org/notes.org" "/home/zjr/Documents/org/productivity.org" "/home/zjr/Documents/org/tensors.org" "/home/zjr/Documents/org/vast.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (ivy-bibtex auctex ox-gfm markdown-mode counsel ivy swiper persp-mode e2wm excorporate htmlize pretty-mode nxhtml ein-mumamo slack ein transpose-frame exwm-surf exwm which-key define-word adaptive-wrap eldoro pomidor org-pomodoro org-variable-pitch smart-mode-line apropospriate-theme org-reveal ox-reveal calfw calfw-org org-doing org-super-agenda aproprospriate aproprospriate-theme badger-theme org-journal buffer-move move-text realgud elpy julia-repl ob-async org-ehtml shell-command helpful magit inline-string-rectangle multiple-cursors mark-more-like-this expand-region ess ob-sh julia-mode org use-package)))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (quote
            (("defexamples\\|def-example-group\\| => "
              (0
               (quote font-lock-keyword-face))))))
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode))
     (eval font-lock-add-keywords nil
           (quote
            (("defexamples\\| => "
              (0
               (quote font-lock-keyword-face))))))
     (encoding . utf-8))))
 '(split-height-threshold 9999))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
;;  '(diff-refine-change ((t (:background "midnight blue"))) t)
;;  '(diff-refine-changed ((t (:background "midnight blue"))))
;;  '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
;;  '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
;;  '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) nil)))
;;  '(show-paren-match ((nil (:background "#333399"))))
;;  '(show-paren-mismatch ((((class color)) (:background "red")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
