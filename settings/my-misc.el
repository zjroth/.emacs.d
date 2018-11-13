;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)
;(remove-hook 'before-save-hook 'cleanup-buffer-safe)

;; Newline after inserting closing tag in html-mode
(defadvice sgml-close-tag (after close-tag-then-newline activate)
  (newline-and-indent))

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

;; Use the system's ssh connection settings.
(setq tramp-use-ssh-controlmaster-options nil)

(provide 'my-misc)
