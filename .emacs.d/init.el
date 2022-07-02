;; TODOs
;; * Straight package manager https://github.com/raxod502/straight.el
;; * Yasnippets + org mode snippets

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(scroll-bar-mode -1) 
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(cond ((eq system-type 'darwin) (set-face-attribute 'default nil :height 180 :font "UbuntuMono Nerd Font"))
      ((eq system-type 'windows-nt) (set-face-attribute 'default nil :height 180 :font "Hack")))

;; When opening a symlink that links to a file in a git repo, edit the file in the
;; git repo so we can use the Emacs vc features (like Diff) in the future
(setq vc-follow-symlinks t)
(setq create-lockfiles nil
      make-backup-files nil)

;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
(setq default-input-method "spanish-postfix")

(setq mac-control-modifier 'control
      mac-right-command-modifier 'control)

(global-set-key (kbd "<escape>") 'keyboard-quit)

(require 'package)
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; END BOOTSTRAPPING

;; Don't really want vterm on windows, win shell is horrible
;; https://github.com/akermu/emacs-libvterm
(unless (eq system-type 'windows-nt)
        (use-package vterm))
(use-package org)
(use-package org-evil
  :after org evil
  :hook (org-mode . evil-mode))


(dolist (mode '(term-mode-hook eshell-mode-hook org-mode-hook help-mode-hook))
        (add-hook mode (lambda() (display-line-numbers-mode 0))))    

(use-package pdf-tools)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-display-line-numbers-mode t)

;; langs
(use-package haskell-mode)
(use-package lua-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x"    . counsel-M-x)
	("C-x b"   . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	("C-x C-b" . counsel-switch-buffer)
	 )) 

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
)

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-snipe
  :diminish evil-snipe-mode
  :diminish evil-snipe-local-mode
  :after evil
  :config
  (evil-snipe-mode +1))


(use-package doom-themes)
;;(load-theme 'doom-tokyo-night t)
(load-theme 'doom-ir-black t)

(use-package ripgrep)
(use-package projectile
 :init
 (projectile-mode +1))
   
(use-package flycheck)
(use-package company)

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package treemacs)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :after treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (let ((lombok-path "/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar"))
    (setq lsp-java-vmargs  '("-noverify"
			     "-Xmx1G"
			     "-XX:+UseG1GC"
			     "-XX:+UseStringDeduplication"
			     "-javaagent:/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar"
			     "-Xbootclasspath/a:/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar")))
  )
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(use-package hydra)
;; It would be nice if we could highlight the border of the current window with some color

(defhydra hydra-split-resizing (:timeout 4)
  "Split resize function for hydra"

   ("<left>" (evil-window-decrease-width 5) "<")
   ("<right>" (evil-window-increase-width 5) ">")
   ("<down>" (evil-window-increase-height 3) "-")
   ("<up>" (evil-window-decrease-height 3) "+")
   ("f" nil "done" :exit t))

;; EL GENERALISIMO
(use-package general
  :after evil projectile hydra lsp-mode
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override

   "C-l" 'evil-window-right
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "H" 'next-buffer
   "L" 'previous-buffer)

  (general-create-definer ddn/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")

  (ddn/leader-keys
    "b"  '(nil  :which-key "buffer")
    "bi" '(counsel-ibuffer  :which-key "ibuffer")
    "bk" '(kill-buffer  :which-key "kill buffer")
    ;;"c"  '(lsp-mode-map  :which-key "code")
    "f"  '(nil  :which-key "find")
    "g"  '(nil  :which-key "git")
    "h"  '(nil  :which-key "help")
    "ha" '(counsel-apropos  :which-key "apropos")
    "hf" '(counsel-describe-function  :which-key "describe function")
    "hk" '(describe-key  :which-key "describe key")
    "hm" '(describe-mode  :which-key "describe mode")
    "hs" '(counsel-describe-symbol  :which-key "describe symbol")
    "hv" '(counsel-describe-variable  :which-key "describe variable")
    "p"  '(projectile-command-map :which-key "project")
    "t"  '(nil  :which-key "toggle")
    "w"  '(nil  :which-key "window")
    "wc" '(evil-window-delete :which-key "close")
    "wr" '(hydra-split-resizing/body :which-key "resize")
))

;; org
;; disable prompts
(setq org-confirm-babel-evaluate nil)
