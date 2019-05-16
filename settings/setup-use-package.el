;; Some of this information was taken from the use-package readme on
;; github, and some was taken from a blog post:
;;     http://cachestocaches.com/2015/8/getting-started-use-package/

(require 'package)
(setq package-enable-at-startup nil)

;; Add package repos and then initialize package.
(add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"       . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Install "use-package" if it is not already installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Remove old package files once a package has been updated.
(setq auto-package-update-delete-old-versions t)

(eval-when-compile
  (require 'use-package))

;; Defaults: use melpa and install missing packages
(setq use-package-always-pin "melpa"    ; same as ":pin melpa" everywhere
      use-package-always-ensure t       ; same as ":ensure t" everywhere
      )

;; Allow :diminish and :bind keywords.
(use-package diminish :demand t)
(use-package bind-key :demand t)

;; Allow loading with "(require 'setup-use-package)".
(provide 'setup-use-package)
