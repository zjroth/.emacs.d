(require 'package)
(setq package-enable-at-startup nil)

;; Add package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Auto-install "use-package"
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; I'm not sure why this is here. See
;; http://cachestocaches.com/2015/8/getting-started-use-package/.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Allow loading with "(require 'setup-use-package)".
(provide 'setup-use-package)
