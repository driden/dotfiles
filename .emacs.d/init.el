
;; TODOs
;; * Disable C-x C-c
;; * Better terminal emulator
;; * Org mode keybindings that exist on Doom
;; * LSP mode
;; * Straight package manager https://github.com/raxod502/straight.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(scroll-bar-mode -1) 
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 180)

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

;; https://github.com/akermu/emacs-libvterm
(use-package vterm)


(dolist (mode '(term-mode-hook eshell-mode-hook org-mode-hook help-mode-hook))
        (add-hook mode (lambda() (display-line-numbers-mode 0))))    

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
  :bind (("M-x"     . counsel-M-x)
	 ("C-x b"   . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file))) 

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
(load-theme 'doom-tokyo-night t)

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
  :after evil
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override

   "C-l" 'evil-window-right
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up

   "<S-h>" 'next-buffer
   "<S-l>" 'previous-buffer)

  (general-create-definer ddn/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")

  (ddn/leader-keys
    "b"  '(nil  :which-key "buffer")
    "bi" '(counsel-ibuffer  :which-key "ibuffer")
    "c"  '(nil  :which-key "code")
    "f"  '(nil  :which-key "find")
    "g"  '(nil  :which-key "git")
    "h"  '(nil  :which-key "help")
    "ha" '(counsel-apropos  :which-key "apropos")
    "hf" '(counsel-describe-function  :which-key "describe function")
    "hm" '(describe-mode  :which-key "describe mode")
    "hs" '(counsel-describe-symbol  :which-key "describe symbol")
    "hv" '(counsel-describe-variable  :which-key "describe variable")
    "p"  '(nil  :which-key "project")
    "t"  '(nil  :which-key "toggle")
    "w"  '(nil  :which-key "window"))
    "wc" '(evil-window-delete :which-key "close")
    "wr" '(hydra-split-resizing/body :which-key "resize")
)

;;  (ddn/help-keys
;;    "a" '(apropos :which-key "apropos")
;;    "f" '(describe-function :which-key "function")
;;    "v" '(describe-variable :which-key "variable"))
;;    "m" '(describe-mode :which-key "mode"))
;;
;;  (ddn/window-keys
;;    "r" '(hydra-split-resizing/body :which-key "resizing")
;;    "c" '(evil-window-delete :which-key "close"))
;;)

;; org
;; disable prompts
(setq org-confirm-babel-evaluate nil)
