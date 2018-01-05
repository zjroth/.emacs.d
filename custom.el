(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(elfeed-feeds (quote ("https://www.mathjobs.org/jobs?joblist-0-----rss")))
 '(ido-use-filename-at-point nil)
 '(org-agenda-files (quote ("~/Dropbox/org/things.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (ivy-rich ox-pandoc which-key org-pomodoro org-attach-screenshot helm helm-org-rifle helm-orgcard csv-mode elfeed elfeed-goodies elfeed-org god-mode load-theme-buffer-local color-theme dracula-theme spacemacs-theme lastpass language-detection org-bullets ob-ipython counsel ivy swiper zoom-frm fill-column-indicator eproject shell-command dired-details undo-tree smooth-scrolling use-package adaptive-wrap yaml-mode yaml-tomato org js-comint zencoding-mode wgrep visual-fill-column tidy tagedit smex smart-forward slime-js rainbow-delimiters perspective paredit multiple-cursors multifiles move-text monokai-theme markdown-mode+ mark-multiple mark-more-like-this magit ledger-mode jump-char ido-ubiquitous hledger-mode helm-ag gitignore-mode gitconfig-mode gist find-file-in-project exec-path-from-shell ess elisp-slime-nav company cider change-inner buster-snippets buffer-move browse-kill-ring)))
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
     (encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(diff-refine-change ((t (:background "midnight blue"))) t)
 '(diff-refine-changed ((t (:background "midnight blue"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) nil)))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))
