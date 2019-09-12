;; To make things work as I like, a few things need to be installed on a mac
;; system.  For me, installing homebrew was necessary.  Once homebrew is
;; installed, run the following commands:
;; - brew install coreutils
;; - brew install aspell --lang=en

;; Fix emacs modifiers on a mac.
(setq mac-command-modifier 'meta
      mac-option-modifier  'super
      mac-control-modifier 'control)

;; This is so that I can use the macbook pro keyboard, which doesn't have a
;; right control key.  Bah!
(setq ns-right-option-modifier 'control)

;; mac friendly font
;;(set-face-attribute 'default nil :font "Monaco-16")

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(provide 'mac)
