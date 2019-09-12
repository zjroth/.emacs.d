
(use-package realgud)

;; An IDE-like experience
(use-package elpy
  :config
  (elpy-enable)

  ;; (setq python-shell-interpreter "python"
  ;;       python-shell-interpreter-args "-i")

  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt --kernel python3"
        python-shell-prompt-detect-failure-warning nil)

  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  )

(use-package python-docstring)

(provide 'use-python)
