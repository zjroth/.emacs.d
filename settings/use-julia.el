(use-package julia-mode
  :defer t)

(use-package julia-repl
  :defer t
  :hook julia-mode
  ;; :config (julia-repl-set-executable
  ;;          (expand-file-name "~/bin/julia"))
  )

;; (setq inferior-julia-program-name "~/bin/julia")
;; (setq inferior-julia-program-name
;;       (expand-file-name "~/bin/julia"))

;; (use-package julia-shell)
(use-package julia-snail)

;; (use-package eat
;;   :pin nongnu
;;   :custom (eat-kill-buffer-on-exit t)
;;
;;   :config
;;   (progn
;;     (delete [?\C-u] eat-semi-char-non-bound-keys) ; make C-u work in Eat terminals like in normal terminals
;;     (delete [?\C-g] eat-semi-char-non-bound-keys) ; ditto for C-g
;;     (eat-update-semi-char-mode-map)
;;     ;; XXX: Awkward workaround for the need to call eat-reload after changing Eat's keymaps,
;;     ;; but reloading from :config section causes infinite recursion because :config wraps with-eval-after-load.
;;     (defvar eat--prevent-use-package-config-recursion nil)
;;     (unless eat--prevent-use-package-config-recursion
;;       (setq eat--prevent-use-package-config-recursion t)
;;       (eat-reload))
;;     (makunbound 'eat--prevent-use-package-config-recursion)))

(use-package vterm
  :defer t
  :ensure t)

(use-package julia-snail
  :ensure t
  :defer t
  :hook (julia-mode . julia-snail-mode))

(provide 'use-julia)
