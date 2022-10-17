;; -*- lexical-binding: t; -*-

; Moving around in buffers
; 
; The most basic buffer movement commands move point (the cursor) by rows (lines) or columns (characters):
; 
;  C-f  Forward one character  
;  C-n  Next line  
; 
;  C-b  Back one character  
;  C-p  Previous line  
; 
; Here are some ways to move around in larger increments:
; 
;  C-a  Beginning of line  
;  M-f  Forward one word  
;  M-a  Previous sentence  
;  M-v  Previous screen  
;  M-<  Beginning of buffer  
; 
;  C-e  End of line  
;  M-b  Back one word  
;  M-e  Next sentence  
;  C-v  Next screen  
;  M->  End of buffer  

(defalias 'yes-or-no-p 'y-or-n-p)
    (setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))
(setq make-backup-files nil)
(setq custom-file (if (eq system-type 'windows-nt)
                      (concat (getenv "APPDATA") "\\.emacs.d\\custom.el")
                    "~/.emacs.d/custom.el"))
(load custom-file)

(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(global-auto-revert-mode 1)

                                        ;(cond ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
                                        ;             (set-face-attribute 'default nil :height 140 :font "Monaco"))
                                        ;            ((eq system-type 'windows-nt)
                                        ;             (set-face-attribute 'default nil :height 140 :font "Hack")))
(set-face-attribute 'default nil :height 150 :font "Hack")
;; When opening a symlink that links to a file in a git repo, edit the file in the
;; git repo so we can use the Emacs vc features (like Diff) in the future
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(setq vc-follow-symlinks t)
(setq
 dired-auto-revert-buffer t
 create-lockfiles nil
 make-backup-files nil
 create-lockfiles nil
 visible-bell nil
 ring-bell-function 'ignore)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

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


;;; PATH
(unless (eq system-type 'windows-nt)
    (use-package exec-path-from-shell :config (exec-path-from-shell-initialize)))
(use-package vterm)

;;; Line numbers
(defun ddn/line-numbers ()
  (display-line-numbers-mode)
  (setq global-display-line-numbers-mode t
        display-line-numbers 'relative))

                                        ; Line Numbers
(setq global-display-line-numbers-mode t
      display-line-numbers 'relative)
(add-hook 'prog-mode-hook 'ddn/line-numbers)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode
                help-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package pdf-tools
  :defer t)

(use-package avy)

(use-package ace-window
  :init 
  (setq aw-keys '(?A ?S ?D ?F ?Q ?W ?R ?Z ?X))

  )
(use-package vertico
  :config
  (vertico-mode))
(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters '((line-spacing . 7)
                                      (border-width . 3)
                                      (left-fringe . 10)
                                      (right-fringe . 10)))
  (vertico-posframe-mode 1))


(use-package consult
  :after vertico
  :config
  (setq consult-narrow-key "<")
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion))))

(use-package marginalia :config (marginalia-mode))

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
;; langs
(use-package json-mode :defer t :hook (json-mode . flycheck-mode))
(use-package haskell-mode :defer t)
(use-package lua-mode :defer t)
(use-package yaml-mode :defer t
  :hook (yaml-mode . ddn/highlight-line))

(use-package terraform-mode
  :defer t
  :hook
  ((terraform-mode . lsp-deferred)
   (terraform-mode . tree-sitter-hl-mode)))

;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package typescript-mode
  :defer t
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


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))


(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.075) 
  (evil-goggles-use-diff-faces))

(use-package ripgrep)
(use-package projectile
  :init
  (projectile-mode +1))

(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'lines))

(use-package company 
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :hook (prog-mode . company-mode))

(use-package company-quickhelp
  :after company)

(use-package company-box
  :after company
  :config
  :hook
  (company-mode . company-box-mode))

(use-package editorconfig
  :defer t
  :hook ((typescript-mode . editorconfig-mode)
         (js-mode . editorconfig-mode)))

(use-package tree-sitter
  :defer t
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-file-watch-threshold 4000)
  :commands (lsp lsp-deferred))

(add-hook 'js-mode-hook #'lsp-deferred)

(use-package dap-mode
  :defer t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package groovy-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(use-package winum :config (winum-mode 1))
(use-package js ;built-in
  :hook (js-mode . tree-sitter-mode))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-RET-actions-config
   '((treemacs-node-closed-state . lsp-treemacs-perform-ret-action)
     (treemacs-node-open-state . lsp-treemacs-perform-ret-action)
     (treemacs-lsp-treemacs-deps-closed-state . lsp-treemacs-deps--goto-element)
     (treemacs-lsp-treemacs-deps-open-state . lsp-treemacs-deps--goto-element)
     (treemacs-lsp-symbol-closed-state . lsp-treemacs-goto-symbol)
     (treemacs-lsp-symbol-open-state . lsp-treemacs-goto-symbol)
     (root-node-open . treemacs-toggle-node)
     (root-node-closed . treemacs-toggle-node)
     (dir-node-open . treemacs-toggle-node)
     (dir-node-closed . treemacs-toggle-node)
     (file-node-open . treemacs-visit-node-ace)
     (file-node-closed . treemacs-visit-node-ace)
     (tag-node-open . treemacs-toggle-node-prefer-tag-visit)
     (tag-node-closed . treemacs-toggle-node-prefer-tag-visit)
     (tag-node . treemacs-visit-node-default)))
                                        ;(setq treemacs-RET-actions-confgi '(file-node-open #'treemacs-visit-node-ace))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :defer t
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile)
  :ensure t)

(defun ddn/dired-single ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [remap dired-find-file] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-single
  :after dired
  :init (ddn/dired-single))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :after treemacs :commands lsp-treemacs-errors-list)

;; Probably need to move this config to custom.el now
(use-package lsp-java
  :defer t
  :after (lsp-mode dap-mode)
  :hook ((java-mode . lsp-deferred) (java-mode . tree-sitter-mode ))
  :config
  (let ((lombok-path "/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar"))
    (setq lsp-java-vmargs  '("-noverify"
                             "-Xmx1G"
                             "-XX:+UseG1GC"
                             "-XX:+UseStringDeduplication"
                             "-javaagent:/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar"
                             "-Xbootclasspath/a:/Users/lrrezend/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.24/13a394eed5c4f9efb2a6d956e2086f1d81e857d9/lombok-1.18.24.jar")))

  (setq dap-java-use-testng t)
  (dap-register-debug-template "DEV"
                               (list :type "java"
                                     :request "launch"
                                     :args ""
                                     :vmArgs ddn/jvm-args
                                     :projectName "myapp"
                                     :mainClass "com.domain.AppRunner"
                                     :env test-args)))

(defvar test-args '())
(defvar ddn/jvm-args  "-ea -DENV=dev -DsocksProxyHost=localhost -DsocksProxyPort=5001 -DREGION=us-east-1")

(use-package hydra :defer t)
;; It would be nice if we could highlight the border of the current window with some color

(defhydra hydra-split-resizing (:timeout 4)
  "Split resize function for hydra"

  ("<left>" (evil-window-decrease-width 5) "<")
  ("<right>" (evil-window-increase-width 5) ">")
  ("<down>" (evil-window-increase-height 3) "-")
  ("<up>" (evil-window-decrease-height 3) "+")
  ("f" nil "done" :exit t))

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
; TODO / dark/light switching
; light = doom-flatwhite
; dark  = doom-ayu-dark 

(defvar ddn/current-theme 'doom-ayu-dark "Current set theme")
(ddn/set-theme ddn/current-theme)

;; -------

(defun ddn/on-windows () (eq system-type 'windows-nt))

(unless (ddn/on-windows)
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)))

(use-package beacon
  :diminish
  :commands beacon-mode
  :init
  (beacon-mode 1)
  :custom
  (beacon-color (face-attribute 'match :foreground)))

(defun ddn/highlight-line () (hl-line-mode t))
(defun ddn/toggle-indent-guideline  ()
  "Toggle showing indentation guidelines"
  (interactive)
  (highlight-indent-guides-mode 'toggle))
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character))


(use-package try :defer t)

(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  (yas-global-mode 1))

;; install useful snippets
(use-package yasnippet-snippets
  :after yasnippet)

;;; ORG
(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (set-face-attribute 'org-level-1 nil :height 1.6)
  (set-face-attribute 'org-level-2 nil :height 1.5)
  (set-face-attribute 'org-level-3 nil :height 1.4)
  (set-face-attribute 'org-level-4 nil :height 1.3)
  (set-face-attribute 'org-level-5 nil :height 1.2)
  (set-face-attribute 'org-level-6 nil :height 1.1)
  (set-face-attribute 'org-document-title nil :height 2.0 :weight 'bold))

(use-package org-contrib :after org)
(use-package org-evil :after (org evil))
;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq line-spacing 0.3
        ;; Edit settings
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"

        ;; Agenda styling
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"))


(defun ddn/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ddn/org-mode-visual-fill))

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
   :keymaps 'vertico-map

   "C-p"   'vertico-scroll-down
   "C-u"   'vertico-scroll-up
   "C-["   'vertico-exit
   "C-p"   'vertico-previous
   "C-n"   'vertico-next)
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
   :states '(normal)
   :keymaps 'override
   :prefix "SPC"
   "s" 'avy-goto-char 
   "S" 'avy-goto-char-2) 

  (general-define-key
   :states '(normal motion visual emacs)
   :keymaps 'override
   :prefix "C-x"
   "h" 'previous-buffer
   "l" 'next-buffer)

  (general-define-key 
   :states '(normal insert)
   :keymaps 'lsp-mode-map

   "C-SPC" '(completion-at-point))

  (general-define-key 
   :states '(normal)
   :keymaps 'lsp-mode-map

   "ga"    '(lsp-execute-code-action :whick-key "go to actions")
   "gd"    '(lsp-find-definition :whick-key "go to definition")
   "gD"    '(lsp-find-declaration :whick-key "go to declaration")
   "gh"    '(nil :whick-key "go hover")
   "gi"    '(lsp-goto-implementation :whick-key "go to implementation")
   "gr"    '(lsp-find-references :whick-key "go to references"))

  (general-define-key 
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC"
   :global-prefix "C-SPC"

   "b"  '(nil  :which-key "buffer")
   "bi" '(ibuffer  :which-key "ibuffer")
   "bk" '(kill-buffer  :which-key "kill buffer")
   "c"  '(nil  :which-key "code")
   "ca" '(lsp-execute-code-action  :which-key "code action")
   "cr" '(lsp-rename  :which-key "rename symbol")
   "cs" '(lsp  :which-key "lsp start")
   "e"  '(nil  :which-key "explore")
   "ee" '(treemacs  :which-key "explore project")
   "f"  '(nil  :which-key "find")
   "ff" '(find-file  :which-key "find file")
   "g"  '(nil  :which-key "git")
   "h"  '(nil  :which-key "help")
   "ha" '(apropos  :which-key "apropos")
   "hf" '(describe-function  :which-key "describe function")
   "hk" '(describe-key  :which-key "describe key")
   "hm" '(describe-mode  :which-key "describe mode")
   "hs" '(describe-symbol  :which-key "describe symbol")
   "hv" '(describe-variable  :which-key "describe variable")

   "o"  '(nil :which-key "org")
   "oc" '(nil :which-key "org-clock")
   "oci"'(org-clock-in :which-key "org-clock-in")
   "oco"'(org-clock-out :which-key "org-clock-out")
   "p"  '(projectile-command-map :which-key "project")
   "t"  '(nil  :which-key "toggle")
   "ti" '(ddn/toggle-indent-guideline  :which-key "Toggle indentation guides")
   "tt" '(ddn/cycle-themes  :which-key "set next theme")
   "w"  '(nil  :which-key "window")
   "wc" '(evil-window-delete :which-key "close")
   "wr" '(hydra-split-resizing/body :which-key "resize")))
