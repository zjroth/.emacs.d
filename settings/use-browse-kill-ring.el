(use-package browse-kill-ring
  :defer t

  :config
  (setq browse-kill-ring-quit-action
        'save-and-restore))

(provide 'use-browse-kill-ring)
