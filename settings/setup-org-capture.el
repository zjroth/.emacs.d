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
:SOURCE:   %a
:END:

%i"
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

        ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
         "
* %<%R> :: %?
:PROPERTIES:
:CAPTURED: %U
:SOURCE:   %a
:END:

%i"
         :clock-in t :clock-resume t :empty-lines 1 :kill-buffer t)

        ("k" "Journal (unclocked)" entry (file+olp+datetree "~/Documents/org/journal.org")
         ;; "* %<%R> %?\n\n%a\n\n"
         ;; :SOURCE:   %a
         "
* %<%I:%M %p>
:PROPERTIES:
:CAPTURED: %U
:END:

%?%i"
         :empty-lines 1 :kill-buffer t :tree-type week)

        ;; ("l" "Log" entry (function org-journal-find-location)
        ;;  "* %(format-time-string org-journal-time-format)%a\n:PROPERTIES:\n  :ORIGIN:   %l\n:END:\n\n%?\n\n")

;;         ("l" "Link" plain (function (lambda ()))
;;          "
;; * %c
;; :PROPERTIES:
;; :CAPTURED: %U
;; :END:
;; %?
;; %:initial"
;;          :clock-in t :clock-resume t :empty-lines 1 :kill-buffer t)
;;
;;         ("w" "Web site" plain (file "")
;;          "* %a :website:\n\n%U %?\n\n%:initial")

        ))

(setq org-roam-capture-templates
      '(("d" "default" plain "#+title: ${title}\n#+category: misc\n\n%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
         :unnarrowed t
         :clock-in t :clock-resume t)
        ("i" "Crescent issue" plain "
#+title: Linear Issue ${title}
#+url: https://linear.app/crescent-financial-inc/issue/${title}
#+category: ${title}

* NEXT Complete issue ${title}
:PROPERTIES:
:CREATED:  %U
:ORDERED:  t
:END:

** TODO %?
:PROPERTIES:
:CREATED:  %U
:TRIGGER:  chain-siblings(NEXT)
:END:
"
;; Description:
;; - Linear title :: %?
;; - Goal ::
;; - Motivation ::
;; - Outcome ::

         :target (file+head "%<%Y%m%d%H%M%S>-linear-issue-${slug}.org" "")
         :unnarrowed t
         :clock-in t :clock-resume t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "\n* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(defun org-capture-journal ()
  (org-capture nil "j")
  (zoom-in/out 6)
  (writeroom-mode))

;; (defun org-capture-journal-in-new-frame ()
;;   (interactive)
;;   (let ((new-frame (make-frame-command)))
;;     (select-frame-set-input-focus new-frame)
;;     (toggle-frame-maximized new-frame)
;;     (org-capture nil "k")
;;     (delete-other-windows)))

(provide 'setup-org-capture)
