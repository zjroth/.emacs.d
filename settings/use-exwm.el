(use-package exwm
  :pin gnu
  :init (defun exwm-start-xrandr ()
          (start-process-shell-command
           "xrandr" nil "xrandr --output DP-1 --right-of DP-2 --auto"))

  :hook ((exwm-randr-screen-change . exwm-start-xrandr))

  :config (progn
            (require 'exwm)
            (require 'exwm-config)
            (exwm-config-default)

            (require 'exwm-randr)
            (setq exwm-randr-workspace-output-plist
                  '(0 "DP-1" 1 "HDMI-1-1" 2 "eDP-1-1"))
            (exwm-randr-enable)))

(use-package exwm-surf
  :after exwm
  :hook ((exwm-manage-finish . exwm-surf-init))
  :config (progn
            (setq exwm-surf-history-file "~/.surf/history")
            (setq exwm-surf-bookmark-file "~/.surf/bookmarks")))

(provide 'use-exwm)
