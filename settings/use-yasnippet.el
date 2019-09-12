;; The package for using/defining snippets.
(use-package yasnippet
  :after ivy

  :init
  (progn
    ;; Use only own snippets, do not use bundled ones
    ;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

    ;; Don't expand yasnippets in every setting
    (setq yas-expand-only-for-last-commands
          '(self-insert-command
            yas-exit-all-snippets
            yas-abort-snippet
            yas-skip-and-clear-or-delete-char
            yas-next-field-or-maybe-expand))

    ;; Inter-field navigation
    (defun yas-goto-end-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas-snippets-at-point)))
             (position (yas-field-end (yas-snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line)
          (goto-char position))))

    (defun yas-goto-start-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas-snippets-at-point)))
             (position (yas-field-start (yas-snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line)
          (goto-char position))))

    ;; No dropdowns please, yas
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

    ;; Wrap around region
    (setq yas-wrap-around-region t))

  :bind (("C-c s" . ivy-yasnippet))

  :bind (:map yas-keymap
              ("C-e" . yas-goto-end-of-active-field)
              ("C-a" . yas-goto-start-of-active-field)
              ;; Jump to end of snippet definition
              ("<return>" . yas-exit-all-snippets))

  :config
  (progn
    (yas-global-mode 1)))

;; Use some pre-defined snippets.
(use-package yasnippet-snippets
  :after yasnippet)

;; Integrate with ivy for selection of snippets.
(use-package ivy-yasnippet
  :after (dash ivy yasnippet)

  :bind ("C-c s" . ivy-yasnippet))

(provide 'use-yasnippet)
