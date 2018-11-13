(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Documents/org/inbox.org")
         "* TODO %?\n  %i\n  %a")
        ("a" "Article" entry (file+olp "~/Documents/org/lists.org" "Reading" "Internet articles")
         "* %c\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%?\n%:initial")
        ;;         ("a" "Article" entry (file+olp "~/Dropbox/org/lists.org" "Reading" "Internet articles")
        ;;          "
        ;; * %c
        ;; :PROPERTIES:
        ;;   :CAPTURED: %U
        ;; :END:
        ;; %?
        ;; %:initial")

        ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")

        ("l" "Log" entry (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%a\n:PROPERTIES:\n  :ORIGIN:   %l\n:END:\n\n%?\n\n")))

(provide 'setup-org-capture)
