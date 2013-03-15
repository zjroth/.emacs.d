(require 'matlab-load)

(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(define-key matlab-mode-map (kbd "M-;") 'comment-dwim)
(define-key matlab-mode-map (kbd "M-j")
  (lambda () (interactive) (join-line -1)))

(provide 'setup-matlab-mode)
