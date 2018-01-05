(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-mode/")
(load-library "matlab-load")

(matlab-cedet-setup)

(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(setq matlab-indent-level 4)

(define-key matlab-mode-map (kbd "M-;") 'comment-dwim)
(define-key matlab-mode-map (kbd "M-j")
  (lambda () (interactive) (join-line -1)))

(provide 'setup-matlab-mode)
