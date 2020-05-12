;; emacs ipython notebook
(use-package ein
  :defer t

  :config
  (require 'ein-notebook)
  (require 'ein-subpackages))

;; (use-package ein-mumamo)

(provide 'use-ein)
