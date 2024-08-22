(use-package realgud)

(add-hook 'python-mode-hook (lambda () (set-fill-column 79)))
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'python-mode-hook 'blacken-mode)

;; An IDE-like experience
(use-package elpy
  :ensure t
  :defer t
  :init
  (progn
    (advice-add 'python-mode :before 'elpy-enable)
    (setenv "WORKON_HOME" "/usr/local/anaconda3/envs"))

  :config
  (progn
    (setq python-shell-interpreter "/opt/homebrew/bin/python3"
          python-shell-interpreter-args "-i")
    (setq elpy-test-runner 'elpy-test-pytest-runner)
    (setq elpy-rpc-python-command "/opt/homebrew/bin/python3")

    ;; (setq python-shell-interpreter "jupyter"
    ;;       python-shell-interpreter-args "console --simple-prompt --kernel python3"
    ;;       python-shell-prompt-detect-failure-warning nil)

    (setq elpy-rpc-virtualenv-path 'current)

    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")))

(setq python-fill-docstring-style 'symmetric)

(use-package python-docstring)

(use-package blacken
  :defer t
  :init
  (setq blacken-line-length 88))

(use-package pip-requirements
  :defer t)

(provide 'use-python)
