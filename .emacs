;; at some point it would be wise to use use-package macro https://github.com/jwiegley/use-package
;; it can also integrate with stright package manager https://github.com/raxod502/straight.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Download a theme
(unless (package-installed-p 'ayu-theme)
  (package-install 'ayu-theme))

(load-theme 'ayu-dark t)
