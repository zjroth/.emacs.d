;; Make sure that we use the system shell path.
(use-package exec-path-from-shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'setup-system-interaction)
