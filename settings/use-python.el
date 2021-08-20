
(use-package realgud)

(add-hook 'python-mode-hook (lambda () (set-fill-column 79)))
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'blacken-mode)

;; An IDE-like experience
(use-package elpy
  :init
  (progn
    (setenv "WORKON_HOME" "/usr/local/anaconda3/envs"))

  :config
  (progn
    (elpy-enable)

    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i")
    (setq elpy-test-runner 'elpy-test-pytest-runner)

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

(provide 'use-python)
