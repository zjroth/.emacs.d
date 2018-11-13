;; NOTE: The code below does not work well.
;;
;;     The colors actually look okay, but the variable `eshell-prompt-regex`
;;     must be set along with `eshell-prompt-function`.  Otherwise, eshell
;;     doesn't really know how to function properly.
;;

;; /home/user/current/path    :-->    ~/current/path
(defun shortened-pwd ()
  (let ((pwd (eshell/pwd))
        (home (getenv "HOME")))
    (if (s-matches? (concat "^" home) pwd)
        (concat "~" (s-chop-prefix home pwd))
      pwd)))

;; Change the prompt function
(setq eshell-prompt-function
      (lambda nil
        (concat
         (propertize (concat (user-login-name) "@" (system-name) ":")
                     'face '(:foreground "green" :weight "bold"))
         (propertize (shortened-pwd)
                     'face '(:foreground "lightblue"))
         "$ ")))

;; I'm not exactly sure...but it might be useful.
(setq eshell-highlight-prompt t)
