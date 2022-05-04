;; TODOs
;; * Disable C-x C-c
;; * Better keybindings
;; * Better terminal emulator
;; * Org mode keybindings that exist on Doom
;; * LSP mode
;; * Straight package manager https://github.com/raxod502/straight.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(scroll-bar-mode t) ;; -1 to turn off
(tool-bar-mode -1)
(tooltip-mode -1)
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

(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-control-modifier 'control
      mac-right-command-modifier 'meta)

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

;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  ;; OFF
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
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
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(global-display-line-numbers-mode t)

(dolist (mode '(term-mode-hook eshell-mode-hook org-mode-hook help-mode-hook))
        (add-hook mode (lambda() (display-line-numbers-mode 0))))    


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

(use-package doom-themes)
(load-theme 'doom-tokyo-night t)

(use-package general
  :config
  (general-create-definer ddn/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix: "C-SPC")

  (ddn/leader-keys
    "t" '(:ignore t :which-key "toggle")
    "b" '(:ignore t :which-key "buffer")
    "c" '(:ignore t :which-key "code")
    "w" '(:ignore t :which-key "window")
    "g" '(:ignore t :which-key "git")))

