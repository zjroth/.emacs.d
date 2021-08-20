
;; Email, baby
(add-to-list 'load-path "~/programs/mu/mu4e")
(require 'setup-mu4e)

;; Access Exchange calendars
(use-package excorporate
  :pin gnu

  :config
  (setq excorporate-configuration
        (cons "zach.roth@mutualofomaha.com"
              "https://outlook.office365.com/EWS/Exchange.asmx")))
