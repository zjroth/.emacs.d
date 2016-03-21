(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (org-update-parent-todo-statistics))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Exporting
(require 'ox-odt nil t)
(require 'ox-md nil t)

;; Indent headlines and content.
(setq org-startup-indented t)

;; I use these commands too much outside of org-mode to not have them here, too.
(define-key org-mode-map (kbd "<C-S-return>") 'open-line-above)
(define-key org-mode-map (kbd "<C-S-down>") 'move-text-down)
(define-key org-mode-map (kbd "<C-S-up>") 'move-text-up)
(define-key org-mode-map (kbd "M-p") 'backward-paragraph)
(define-key org-mode-map (kbd "M-n") 'forward-paragraph)

;; Stuff for reproducible research
;(add-to-list 'org-src-lang-modes ...)
;(setq org-src-preserve-indentation t)

;; Execute code blocks in these languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (emacs-lisp . t)
    (sh . t)
    ;(julia . t)
    ;(python . t)
    ;(R . t)
    ;(matlab . t)
    ))

;; Don't ask me to confirm evaluation every time.
(setq org-confirm-babel-evaluate nil)

;; Built-in exporters
(require 'ox-md)
(require 'ox-odt)
(require 'ox-man)
(require 'ox-pandoc)

;; Extra features for exporting.
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks
                      ignore-headlines))

(provide 'setup-org)
