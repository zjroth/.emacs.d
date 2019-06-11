(setq org-capture-templates
      '(
        ;; ("t" "Todo" entry (file "~/Documents/org/inbox.org")
        ;;  "* TODO %?\n  %i\n  %a"
        ;;  :clock-in t :clock-resume t)
        ("t" "Todo" entry (file "~/Documents/org/inbox.org")
         "
* TODO %?
:PROPERTIES:
:CAPTURED: %U
:END:
%i
%a"
         :clock-in t :clock-resume t :empty-lines 1 :kill-buffer t)

        ;; ("a" "Article" entry (file+olp "~/Documents/org/lists.org" "Reading" "Internet articles")
        ;;  "* %c\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%?\n%:initial"
        ;;  :immediate-finish t)
        ("a" "Article" entry (file+olp "~/Dropbox/org/lists.org" "Reading" "Internet articles")
         "
* %c
:PROPERTIES:
:CAPTURED: %U
:END:
%?
%:initial"
         :clock-in t :clock-resume t :empty-lines 1 :kill-buffer t)

        ;; ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
        ;;  "* %?\nEntered on %U\n  %i\n  %a"
        ;;  :empty-lines 1)
        ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
         "
* %?
:PROPERTIES:
:CAPTURED: %U
:END:
%i
%a"
         :clock-in t :clock-resume t :empty-lines 1 :kill-buffer t)

        ;; ("l" "Log" entry (function org-journal-find-location)
        ;;  "* %(format-time-string org-journal-time-format)%a\n:PROPERTIES:\n  :ORIGIN:   %l\n:END:\n\n%?\n\n")
        ))

(provide 'setup-org-capture)
