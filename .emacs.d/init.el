;; Todos
;; * add Eval buffer/last sexp keymaps hooked into emacs-lisp-mode
;; * Yasnippets + org mode snippets

(setq make-backup-files nil)
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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(setq vc-follow-symlinks t)
(setq create-lockfiles nil
      make-backup-files nil
      create-lockfiles nil
      visible-bell t)
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

(use-package beacon
  :diminish
  :commands beacon-mode)

;;; ORG
 (defun ddn/org-bullets ()
    (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))
(use-package org
  :hook (org-mode . ddn/org-bullets)
  :config (setq org-confirm-babel-evaluate nil))

(use-package org-contrib :after org)
(use-package org-evil
  :after (org evil))

(use-package org-bullets

(defun ddn/highlight-line (
  hl-line-mode t)
 (add-hook 'prog-mode-hook #'ddn/highlight-line)
