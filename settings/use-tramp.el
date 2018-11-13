;; Edit remote files
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(provide 'use-tramp)
