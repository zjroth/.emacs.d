
;; (load "auctex.el" nil t t)
(use-package tex
  :ensure auctex
  :pin gnu

  :init
  (progn
    ;; Make sure that luatex is used by default.
    (setq-default TeX-engine 'luatex)
    (setq TeX-command-extra-options "--shell-escape"))

  :config
  (progn
    (require 'reftex)

    ;; Forward/inverse search in LaTeX
    (eval-after-load 'tex-mode '(TeX-source-correlate-mode))

    ;; Parse file after loading it if no style hook is found for it.
    (setq TeX-parse-self t)))

(use-package ivy-bibtex
  ;; Use `ivy-bibtex-with-local-bibliography` to start a search in the current
  ;; buffer’s "local bibliography" using reftex.
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  )
