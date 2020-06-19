
(use-package realgud)

(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 79)))

;; An IDE-like experience
(use-package elpy
  :config
  (elpy-enable)

  ;; (setq python-shell-interpreter "python"
  ;;       python-shell-interpreter-args "-i")

  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt --kernel python3"
        python-shell-prompt-detect-failure-warning nil)

  (setq elpy-rpc-virtualenv-path 'current)

  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  )

(use-package python-docstring)

(provide 'use-python)
