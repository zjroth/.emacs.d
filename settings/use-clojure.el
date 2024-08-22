(use-package clojure-mode
  :hook ((clojure-mode . (lambda () (setq fill-column 100)))
         (clojure-mode . display-fill-column-indicator-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . electric-pair-mode))
  ;; :init
  ;; (add-hook 'clojure-mode-hook (lambda () (setq fill-column 100)))
  ;; (add-hook 'clojure-mode-hook 'display-fill-column-indicator-mode)
  ;; ;; (remove-hook 'clojure-mode-hook (lambda () (visual-fill-column-mode 'disable)))
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  ;; (add-hook 'clojure-mode-hook 'electric-pair-mode)
  :config
  ;; (add-to-list 'clojure-align-binding-forms "mlet" 'append)
  ;; (add-to-list 'clojure-align-binding-forms "rlet" 'append)
  (define-clojure-indent
    ;; Defined like "let": (clojure--get-indent-method "let")
    (alet 1)
    (elet 1)
    (mlet 1)
    (rlet 1)
    (match 1)
    ;; Fulcro Native
    (b/ui-center 1)
    (b/ui-container 1)
    (b/ui-box 1)
    (b/ui-form-control 1)
    (b/ui-stack 1)
    (b/ui-input 1)
    (b/ui-hstack 1)
    (b/ui-tab-view 1)
    (ui-bbutton 1))
  (setq clojure-align-binding-forms (append clojure-align-binding-forms
                                            '("alet" "elet" "mlet" "rlet"))))

(use-package cider
  :hook ((cider-repl-mode . electric-pair-mode))
  :init
  (setq cider-repl-history-file
        (expand-file-name ".cider-repl-history" home-dir))
  (setq cider-repl-history-size 10000)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-prompt-function
        (lambda (namespace) (format "\n%s> " namespace)))
  (setq cider-repl-result-prefix "\n")

  (defun ivy-cider-repl-history ()
    (interactive)
    (ivy-read "Select a previous REPL command: "
              cider-repl-input-history
              :require-match t
              ;; :update-fn (lambda () ...)
              :action '(1 ;; index (1 based) of the default action
                        ("s" (lambda (selection)
                               (end-of-buffer)
                               ;; (cider-repl-delete-current-input)
                               (cider-repl--replace-input selection))))))

  (defun ivy-cider-insert-ns ()
    (interactive)
    (ivy-read "Select namespace to insert: "
              (cider-sync-request:ns-list)
              :require-match t
              :action '(1 ;; index (1 based) of the default action
                        ("s" (lambda (selection)
                               (insert selection))))))
  (defun cider-quit-all ()
    (interactive)
    (mapcar (lambda (session)
              (cider-quit (car (last session))))
            (cider-sessions)))

  :config
  ;; (setq cider-clojure-cli-aliases nil)
  (setq cider-clojure-cli-aliases ":dev:test:cider")
  ;; (setq cider-clojure-cli-global-options "-A:dev:test")
  (setq cider-clojure-cli-global-options nil)
  (setq cider-clojure-cli-parameters nil) ; "-M:cider:dev:test"

  ;; (setq cider-clojure-cli-aliases ":dev:test")
  ;; (setq cider-clojure-cli-global-options "-M:dev:test") ; obsolete

  ;; (setq nrepl-repl-buffer-name-template
  ;;       (concat "*cider-repl (" (getenv "CRESCENT_ENV") ") %s(%r:%S)*"))
  ;; (setq nrepl-repl-buffer-name-template "*cider-repl %s(%r:%S)*")
  (setq nrepl-repl-buffer-name-template nil)
  (defun nrepl-repl-buffer-name (params &optional dup-ok)
    "Return the name of the repl buffer.
PARAMS and DUP-OK are as in `nrepl-make-buffer-name'."
    (nrepl-make-buffer-name (concat "*cider-repl (" (getenv "CRESCENT_ENV") ") %s(%r:%S)*")
                            params dup-ok))

  ;; If I'm highlighting something, I want to be able to overwrite it.
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-forward-delete 'delete-selection 'supersede)

  :bind (:map cider-repl-mode-map
              ("M-s" . nil)
              ("M-r" . nil)
              ("C-r" . ivy-cider-repl-history))
  :bind (:map cider-inspector-mode-map
              ("n" . cider-inspector-next-inspectable-object)
              ("p" . cider-inspector-previous-inspectable-object)
              ("b" . cider-inspector-pop)
              ("t" . toggle-truncate-lines)))

(use-package clj-refactor
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

;; (use-package lsp-mode
;;   :hook ((clojure-mode . lsp)
;;          (clojurec-mode . lsp)
;;          (clojurescript-mode . lsp))
;;   :custom
;;   ((lsp-clojure-server-command '("java" "-jar" "/Users/zroth/programs/clj-kondo/clj-kondo-lsp-server-2022.09.08-standalone.jar")))
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   (setenv "PATH" (concat
;;                   "/usr/local/bin" path-separator
;;                   (getenv "PATH")))
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;;
;;   ;; ;; Optional: In case `clojure-lsp` is not in your $PATH
;;   ;; (setq lsp-clojure-server-command '("/path/to/clojure-lsp"))
;;   )

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; (use-package lsp-treemacs)

;; (use-package eglot)

(provide 'use-clojure)
