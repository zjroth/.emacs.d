(use-package expand-region
  :defer t

  :bind (;; increases selected region by semantic units
         ("C-=" . er/expand-region)))

(provide 'use-expand-region)
