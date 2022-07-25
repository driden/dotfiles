;; Todos
;; * add Eval buffer/last sexp keymaps hooked into emacs-lisp-mode
;; * Yasnippets + org mode snippets

(setq custom-file (if (eq system-type 'windows-nt)
		    (concat (getenv "APPDATA") "\\.emacs.d\\custom.el")
		    "~/.emacs.d/custom.el"))
(load custom-file)

(if (eq system-type 'darwin)
		 (setq ns-function-modifier 'super))
(scroll-bar-mode -1) 
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(cond ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
			 (set-face-attribute 'default nil :height 180 :font "UbuntuMono Nerd Font"))
			((eq system-type 'windows-nt)
			 (set-face-attribute 'default nil :height 180 :font "Hack")))

;; When opening a symlink that links to a file in a git repo, edit the file in the
;; git repo so we can use the Emacs vc features (like Diff) in the future
(setq vc-follow-symlinks t)
(setq create-lockfiles nil
      make-backup-files nil
      create-lockfiles nil
      visible-bell t)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(setq-default tab-width 2)

;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
(setq default-input-method "latin-prefix")

(setq mac-control-modifier 'control
      mac-right-command-modifier 'control)

(global-set-key (kbd "<escape>") 'keyboard-quit)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;; END BOOTSTRAPPING


;;; Evil
(use-package evil
  :init
  (setq evil-want-integration t
				evil-want-C-u-scroll t
				evil-want-Y-yank-to-eol t
				evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package org :config (setq org-confirm-babel-evaluate nil))
(use-package org-contrib :after org)
(use-package org-evil
  :after (org evil)
  :hook (org-mode . evil-mode))

;;(use-package ox-clip :after org)

(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))

; Line Numbers

(setq global-display-line-numbers-mode t
			display-line-numbers 'relative)

(dolist (mode '(shell-mode-hook
								term-mode-hook
								eshell-mode-hook
								help-mode-hook))
				(add-hook mode (lambda() (display-line-numbers-mode -1))))

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
		(ivy-mode 1)
		(setq ivy-initial-inputs-alist nil)
		(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; langs
(use-package json-mode
:hook (json-mode . flycheck-mode))
(use-package yaml-mode)
(use-package haskell-mode)
(use-package lua-mode)

;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package typescript-mode
	:hook (typescript-mode . lsp-deferred)
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
	:after (ivy)
	:init
	(setq ivy-rich-path-style 'abbrev
				ivy-virtual-abbreviate 'full)
  :config
		(ivy-rich-mode 1))
(use-package counsel
  :bind (("M-x"    . counsel-M-x)
				("C-x C-b" . counsel-ibuffer)
				("C-x C-f" . counsel-find-file)
				("C-x b"   . counsel-switch-buffer))) 

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


(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-snipe
  :diminish evil-snipe-mode
  :diminish evil-snipe-local-mode
  :after evil
  :config
  (evil-snipe-mode +1))
(use-package evil-goggles
	:config
	(evil-goggles-mode)
	(evil-goggles-use-diff-faces))
(custom-set-faces
 '(evil-goggles-delete-face ((t (:inherit 'shadow))))
 '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
 '(evil-goggles-yank-face ((t (:inherit 'isearch-fail)))))

(use-package ripgrep)
(use-package projectile
 :init
 (projectile-mode +1))
   
(use-package flycheck)
(use-package company)

(use-package editorconfig-mode)

(use-package tree-sitter
	:config (global-tree-sitter-mode)
	:hook (tree-sitter-hl-mode))

(use-package tree-sitter-langs
	:after tree-sitter)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package lsp-mode
  :init
		(setq lsp-keymap-prefix "C-c l"
					lsp-signature-doc-lines 5
					lsp-idle-delay 0.5)
  :config
	  (lsp-enable-which-key-integration t)
  :commands (lsp lsp-deferred))

(use-package treemacs)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :after treemacs :commands lsp-treemacs-errors-list)
;; Probably need to move this config to custom.el now
(use-package lsp-java
  :hook (java-mode . lsp)
  :config
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
	:demand t
	:init
		(setq general-override-states '(insert
																		emacs
																		hybrid
																		normal
																		visual))
		(general-define-key
				:states '(normal motion visual)
				:keymaps 'override

				"C-l" 'evil-window-right
				"C-h" 'evil-window-left
				"C-j" 'evil-window-down
				"C-k" 'evil-window-up
				"H"   'next-buffer
				"L"   'previous-buffer)

		(general-define-key
				:states '(normal motion visual emacs)
				:keymaps 'override
				:prefix "C-x"
				"C-c" nil) ;; I always quit emacs by accident

		(general-define-key
				:states '(normal insert)
				:keymaps 'company-mode-map
				"C-n" 'company-select-next
				"C-p" 'company-select-previous
				"TAB" 'company-complete-selction)

		(general-define-key 
				:states '(normal)
				:keymaps 'lsp-mode-map

				"C-SPC" '(completion-at-point)
				"gd"    '(lsp-find-definition :whick-key "go to definition")
				"gD"    '(lsp-find-declaration :whick-key "go to declaration")
				"gi"    '(lsp-goto-implementation :whick-key "go to implementation")
				"gr"    '(lsp-find-references :whick-key "go to references"))

;		(general-define-key 
;				:states '(insert)
;				:keymaps 'lsp-mode-map
;				"C-SPC" '(completion-at-point))

		(general-define-key 
				:states '(normal visual emacs)
				:keymaps 'override
				:prefix "SPC"
				:global-prefix "C-SPC"

				"b"  '(nil  :which-key "buffer")
				"bi" '(counsel-ibuffer  :which-key "ibuffer")
				"bk" '(kill-buffer  :which-key "kill buffer")
				"c"  '(nil  :which-key "code")
				"ca" '(lsp-execute-code-action  :which-key "code action")
				"cr" '(lsp-rename  :which-key "rename symbol")
				"cs" '(lsp  :which-key "lsp start")
				"e"  '(nil  :which-key "explore")
				"ee" '(treemacs  :which-key "explore project")
				"f"  '(nil  :which-key "find")
				"ff" '(counsel-find-file  :which-key "find file")
				"g"  '(nil  :which-key "git")
				"h"  '(nil  :which-key "help")
				"ha" '(counsel-apropos  :which-key "apropos")
				"hf" '(counsel-describe-function  :which-key "describe function")
				"hk" '(describe-key  :which-key "describe key")
				"hm" '(describe-mode  :which-key "describe mode")
				"hs" '(counsel-describe-symbol  :which-key "describe symbol")
				"hv" '(counsel-describe-variable  :which-key "describe variable")
				"o"  '(nil :which-key "org")
				"oc" '(nil :which-key "org-clock")
				"oci"'(org-clock-in :which-key "org-clock-in")
				"oco"'(org-clock-out :which-key "org-clock-out")
				"p"  '(projectile-command-map :which-key "project")
				"t"  '(nil  :which-key "toggle")
				"tt" '(ddn/cycle-themes  :which-key "set next theme")
				"w"  '(nil  :which-key "window")
				"wc" '(evil-window-delete :which-key "close")
				"wr" '(hydra-split-resizing/body :which-key "resize")))

;; THEMES
(use-package doom-themes)
(setq ddn/available-themes (custom-available-themes))

(defun ddn/next-theme ()
  "Get the next valid theme from the list."
  (let* ((themes-list   ddn/available-themes)
				 (start-theme   ddn/current-theme)
         (current-theme ddn/current-theme))

				(setq current-theme
						(nth (mod (1+ (cl-position current-theme themes-list))
											(length themes-list))
									themes-list))
   current-theme))

(defun ddn/set-theme (theme)
		(disable-theme ddn/current-theme)
		(setq ddn/current-theme theme)(message "%s" theme)
    (load-theme theme t))

(defun ddn/cycle-themes ()
  "Cycle to the next theme."
  (interactive)
  (let ((new-theme (ddn/next-theme))
        (current-theme 'ddn/current-theme))
				(ddn/set-theme new-theme)))

(setq ddn/current-theme 'doom-old-hope)
(ddn/set-theme 'doom-old-hope)

