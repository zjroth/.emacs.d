(use-package markdown-mode
  :defer t
  :init
  (setq markdown-command "pandoc --to HTML")

  :config
  ;; (defun markdown-filter (buffer)
  ;;   (princ
  ;;    (with-temp-buffer
  ;;      (let ((tmpname (buffer-name)))
  ;;        (set-buffer buffer)
  ;;        (set-buffer (markdown tmpname)) ; the function markdown is in `markdown-mode.el'
  ;;        (buffer-string)))
  ;;    (current-buffer)))
  )

(use-package impatient-showdown
  :defer t
  :config
  (setq impatient-showdown-flavor 'github)
  (setq impatient-showdown-markdown-background-color "#EEEEEE"
        impatient-showdown-background-color "#333333"
        impatient-showdown-markdown-border-color "#EEEEEE"))

(provide 'use-markdown-mode)
