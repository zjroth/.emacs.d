
(use-package dired
  :ensure wdired
  :pin manual

  :init (progn
          ;; Reload dired after making changes
          (--each '(dired-do-rename
                    dired-create-directory
                    wdired-abort-changes)
            (eval `(defadvice ,it (after revert-buffer activate)
                     (revert-buffer))))

          ;; C-a is nicer in dired if it moves back to start of files
          (defun dired-back-to-start-of-files ()
            (interactive)
            (dired-move-to-filename))

          ;; M-up is nicer in dired if it moves to the third line - straight to the ".."
          (defun dired-back-to-top ()
            (interactive)
            (beginning-of-buffer)
            (next-line 2)
            (dired-back-to-start-of-files))

          ;; M-down is nicer in dired if it moves to the last file
          (defun dired-jump-to-bottom ()
            (interactive)
            (end-of-buffer)
            (next-line -1)
            (dired-back-to-start-of-files))

          (eval-after-load "wdired"
            '(progn
               (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
               (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
               (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))))

  :bind (:map dired-mode-map
              ("C-a"     . dired-back-to-start-of-files)
              ("N"       . dired-next-marked-file)
              ("P"       . dired-prev-marked-file)
              ("C-x C-k" . dired-do-delete)  ; to match file buffers and magit

              ([remap smart-up]            . dired-back-to-top)
              ([remap smart-down]          . dired-jump-to-bottom)
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer]       . dired-jump-to-bottom))

  ;; :bind (:map wdired-mode-map
  ;;             ("C-a"                       . dired-back-to-start-of-files)
  ;;             ([remap beginning-of-buffer] . dired-back-to-top)
  ;;             ([remap end-of-buffer]       . dired-jump-to-bottom))

  :config (progn
            ;; List directories first.
            (setq dired-listing-switches "-alh --group-directories-first")

            (when is-mac
              ;; Use the `ls` from gnu-coreutils if it exists.
              (if (shell-command-exists-p "gls")
                  (setq insert-directory-program "gls"
                        dired-use-ls-dired t)
                ;; Otherwise, don't group directories first.
                (setq dired-listing-switches "-alh"))))
  )

;; This is no longer available.  The implementation in emacs 24.4+ does not allow
;; me to set `dired-details-hidden-string`, and it sets "(" to toggle the mode
;; (instead of using "(" and ")" to hide and show details, respectively.
;;
;; ;; Make dired less verbose
;; (use-package dired-details
;;   :config
;;   (setq-default dired-details-hidden-string "--- ")
;;   (dired-details-install))

;; Allow loading of this via a call to `require'.
(provide 'use-dired)
