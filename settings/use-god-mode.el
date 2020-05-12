(use-package god-mode
  :defer t

  ;; (global-set-key (kbd "<escape>") 'god-local-mode)
  :bind (("<escape>" . god-mode-all))

  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

(provide 'use-god-mode)
