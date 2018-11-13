(use-package slack
  :commands (slack-start)

  :init
  (progn
    (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t))

  :config
  (progn
    (slack-register-team
     :name "dunderdata"
     :default t
     :client-id "31522277703.416273804384"
     :client-secret "8a56d0da93f17af755d0e2dcfa9efe86"
     :token "xoxs-31522277703-416850696227-416752961316-002c64e7e4ef7ca26ab7bb09c01adc1d95939c6a10025afb820ee795f937e23b"
     :subscribed-channels '(20180813dc)
     :full-and-display-names t))
   )

(provide 'use-slack)
