(use-package ivy
  :demand t

  :diminish (ivy-mode . "")

  :bind (:map ivy-mode-map
              ("C-'" . ivy-avy))

  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)

    ;; number of result lines to display
    (setq ivy-height 10)

    ;; Display the candidate count and the current candidate.
    (setq ivy-count-format "(%d of %d) ")

    ;; no regexp by default
    (setq ivy-initial-inputs-alist nil)

    ;; configure regexp engine.
    (setq ivy-re-builders-alist
          ;; allow input not in order
          '((t . ivy--regex-ignore-order)))

    ;; Indent lines and put an arrow in front of the current match.
    ;; ▬
    ;; ▶
    ;; ▬▶
    (defun ivy-format-function-arrow (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat "▶ " (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat "  " str))
       cands
       "\n"))

    ;; How to separate org headlines when displaying full paths with ivy.
    (setq counsel-org-headline-path-separator " ▶ ")

    (setq ivy-format-function 'ivy-format-function-arrow)
    (setq ivy-use-selectable-prompt t))

  :bind (("C-x b"     . ivy-switch-buffer)
         ("M-y"       . counsel-yank-pop)
         ("M-x"       . counsel-M-x)
         ("C-s"       . swiper)
         ("C-x C-f"   . counsel-find-file)
         ("C-c C-o"   . ivy-occur))

  :bind (:map org-mode-map
              ("C-c C-j"   . counsel-org-goto-all)
              ;; ("C-h a"     . helm-apropos)
              ;; ("C-x b"     . helm-buffers-list)
              ;; ("C-x c o"   . helm-occur)
              ;; ("C-x c SPC" . helm-all-mark-rings)
              )
  )

(use-package ivy-rich
  :after (:all ivy counsel)

  :init
  (setq ivy-rich-parse-remote-file-path t)
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer)

  :config
  (ivy-rich-mode 1)
  )

(provide 'use-ivy)