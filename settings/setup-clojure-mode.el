(require 'clojure-mode)

(define-key clojure-mode-map (kbd "C-c C-j") 'clj-jump-to-other-file)
(define-key clojure-mode-map (kbd "C-c M-j") 'clj-jump-to-other-file-other-window)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(eval-after-load "nrepl"
  '(progn
     (define-key nrepl-mode-map (kbd "C-,") 'complete-symbol)
     (define-key nrepl-interaction-mode-map (kbd "C-,") 'complete-symbol)))

;; ----------------------------------------------------------------------
;; From http://fgiasson.com/blog/index.php/2016/06/14/my-optimal-gnu-emacs-settings-for-developing-clojure-revised/

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; Turn on auto-completion with Company-Mode
;; (global-company-mode)
;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(provide 'setup-clojure-mode)
