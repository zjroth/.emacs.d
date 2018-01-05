(require 'mu4e)

;; (setq mu4e-mu-binary "~/mu")
(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir "~/mail")

(setq mu4e-get-mail-command "offlineimap")
(setq message-kill-buffer-on-exit t)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Home (zjroth@gmail.com)"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/zjroth" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder  . "/zjroth/[Gmail].Trash")
                   (mu4e-sent-folder   . "/zjroth/[Gmail].Sent Mail")
                   (mu4e-refile-folder . "/zjroth/[Gmail].Archive")
                   (mu4e-drafts-folder . "/zjroth/[Gmail].Drafts")
                   ))

         ,(make-mu4e-context
           :name "Work (zjroth.math@gmail.com)"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/zjrothmath" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder  . "/zjrothmath/[Gmail].Trash")
                   (mu4e-sent-folder   . "/zjrothmath/[Gmail].Sent Mail")
                   (mu4e-refile-folder . "/zjrothmath/[Gmail].Archive")
                   (mu4e-drafts-folder . "/zjrothmath/[Gmail].Drafts")
                   ))
         ))

;; (setq mu4e-maildir "~/mail/zjroth")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; don't prompt for applying of marks, just apply
(setq mu4e-headers-leave-behavior 'apply)

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      mu4e-html2text-command "html2text -utf8 -width 72")

;; Start mu4e in fullscreen, immediately ping for new mail
(defun mu4e-up-to-date-status ()
  (interactive)
  (window-configuration-to-register :mu4e-fullscreen)
  (mu4e)
  (mu4e-update-mail-show-window)
  (delete-other-windows))

;; Restore previous window configuration
(defun mu4e-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :mu4e-fullscreen))

(define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)
(define-key mu4e-headers-mode-map (kbd "M-u") 'mu4e-update-mail-show-window)

;; org-mode integration (links)
(require 'org-mu4e) ;;store org-mode links to messages
(setq org-mu4e-link-query-in-headers-mode nil) ;;store link to message if in header view, not to header query

;; (setq mu4e-html2text-command "html2markdown --body-width=0")
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 80)

(provide 'setup-mu4e)
