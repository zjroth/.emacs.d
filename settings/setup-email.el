
;; Email, baby
(add-to-list 'load-path "~/programs/mu/mu4e")
(require 'setup-mu4e)

;; Access Exchange calendars
(use-package excorporate
  :config
  (setq excorporate-configuration
        (cons "zachary.j.roth@erdc.dren.mil"
              "https://webmail.erdc.dren.mil/owa/")))
