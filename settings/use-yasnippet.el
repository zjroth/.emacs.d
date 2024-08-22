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
    (setq yas-prompt-functions '(ivy-read yas-ido-prompt yas-completing-prompt))
    ;; (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

    ;; Wrap around region
    (setq yas-wrap-around-region t)

    ;; Indentation was off with this function:
    ;; file:~/.emacs.d/elpa/yasnippet-snippets-20200425.1210/snippets/python-mode/.yas-setup.el
    (defun python-args-to-docstring-numpy ()
      "return docstring format for the python arguments in yas-text"
      (let* ((indent (concat "\n" (make-string (current-column) 32)))
             (args (python-split-args yas-text))
             (format-arg (lambda(arg)
                           (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional"))))
             (formatted-params (mapconcat format-arg args indent))
             (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
        (unless (string= formatted-params "")
          (mapconcat 'identity
                     (list (concat indent "Parameters" indent "----------") formatted-params
                           (concat indent "Returns" indent "-------") formatted-ret)
                     indent)))))

  :config
  (progn
    ;; (let ((arg-regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:: *\\([a-zA-Z_][a-zA-Z0-9_\\[\\]]*\\)\\)?\\(?: *= *\\([^,]*\\)\\)?")
    ;;       (arg-list "a_1: Union[str, int] = \"54000\", b2: int, _c=None"))
    ;;   (string-match arg-regexp arg-list)
    ;;   (match-string 2 arg-list))
    ;;
    ;; (defun python-split-args (arg-string)
    ;;   "Split a python argument string into ((name, default)..) tuples"
    ;;   (mapcar (lambda (x)
    ;;             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
    ;;           (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

    ;; (defun python-args-to-docstring-numpy ()
    ;;   "return docstring format for the python arguments in yas-text"
    ;;   (let* ((indent (concat "\n" (make-string (current-column) 32)))
    ;;          (args (python-split-args yas-text))
    ;;          (format-arg (lambda(arg)
    ;;                        (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional"))))
    ;;          (formatted-params (mapconcat format-arg args indent))
    ;;          (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    ;;     (unless (string= formatted-params "")
    ;;       (mapconcat 'identity
    ;;                  (list (concat indent "Parameters" indent "----------") formatted-params
    ;;                        (concat indent "Returns" indent "-------") formatted-ret)
    ;;                  indent))))
    )

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
