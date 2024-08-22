;; Ivy looks for smex and integrates with it.
(use-package smex
  :demand t)

(use-package ivy
  :demand t

  :diminish (ivy-mode . "")

  :bind (:map ivy-mode-map
              ("C-'" . ivy-avy))

  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)

    ;; number of result lines to display
    (setq ivy-height 10)

    ;; Display the candidate count and the current candidate.
    (setq ivy-count-format "(%d of %d) ")

    ;; no regexp by default
    ;; (setq-default ivy-initial-inputs-alist nil)  ; set when loading counsel
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
         ("C-c C-o"   . ivy-occur)))

(use-package counsel
  :defer t
  :config
  (setq-default ivy-initial-inputs-alist nil)

  :bind (("M-s s" . counsel-ag)))

(use-package counsel-ag-popup
  :init
  (progn
    (defun counsel-ag-popup-search-git-root (&optional string)
      (interactive)
      (counsel-ag-popup-search nil string))

    (defun counsel-ag-popup-search-git-src (&optional string)
      (interactive)
      (counsel-ag-popup-search (expand-file-name "src" (counsel--git-root)) string)))

  :bind (("M-s M-s" . counsel-ag-popup))

  :config
  (progn
    (transient-replace-suffix 'counsel-ag-popup "s"
      '("r" "in git root" counsel-ag-popup-search-git-root))
    (transient-append-suffix 'counsel-ag-popup "r"
      '("s" "in git src directory" counsel-ag-popup-search-git-src))
    (transient-insert-suffix 'counsel-ag-popup "o"
      '("h" "in current directory" counsel-ag-popup-search-here))))

(use-package ivy-rich
  ;; From readme for all-the-icons-ivy-rich: "For better performance, enable
  ;; all-the-icons-ivy-rich-mode before ivy-rich-mode."
  :after (:all ivy counsel all-the-icons-ivy-rich)

  :init
  (setq ivy-rich-parse-remote-file-path t)
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer)

  :config
  (ivy-rich-mode 1))

(provide 'use-ivy)
