;;; init.el -- Jono's Emacs configuration

;; Copyright (c) 2020 Jonathan Wheelhouse

;; Author: Jonathan Wheelhouse <jonathan.wheelhouse@gmail.com>
;; URL: https://github.com/JonathanWheelhouse/emacs-config

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; Performance tweaks for modern machines
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Edit configuration
(defun config-visit ()
  "Edit config.org."
  (interactive)
  (find-file "~/.emacs.d/init.el" )
  )
(global-set-key (kbd "C-c e") 'config-visit)

;; Reload configuration
(defun config-reload ()
  "Reload config.org at runtime."
  (interactive)
  (when (file-readable-p "~/.emacs.d/init.el")
    (load-file (expand-file-name "~/.emacs.d/init.el" ))
    )
  )
(global-set-key (kbd "C-c r") 'config-reload)

;; windows only stuff
(when (string-equal system-type "windows-nt")
  (setenv "PATH" (concat "C:/Program Files/Git/bin" ";" (getenv "PATH")))
  (setq exec-path (append '("C:/Program Files/Git/bin") exec-path))
  (setenv "PATH" (concat "C:/Program Files/Git/usr/bin" ";" (getenv "PATH")))
  (setq exec-path (append '("C:/Program Files/Git/usr/bin") exec-path)))

;; proxy.asx.com.au : 8083
;; use cntlm or fiddler
(when (string-equal system-type "windows-nt")
  (defvar url-proxy-services)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ;; ("http" . "localhost:53128") ; cntlm
          ;; ("https" . "localhost:53128")))) ; cntlm
          ("http" . "127.0.0.1:8888") ; fiddler
          ("https" . "127.0.0.1:8888")))) ; fiddler

;; Set the font. Note: height = px * 100
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Consolas" :height 120)
       )
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "Inconsolata" :height 220)
       ))

;; Set up package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-enable-imenu-support t)

(package-initialize)

;; Configure package updates
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 5)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq make-backup-files nil)

(setq frame-title-format "%b")

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(column-number-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; tabs
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ; or any other preferred value

(set-default 'truncate-lines t) ;; Don't wrap long lines.

(setq require-final-newline t) ;; Newline at end of file

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; eye candy
;; 1st time
;; M-x all-the-icons-install-fonts
;; Then install those icons on Windows by double clicking and choosing install
(use-package all-the-icons
  :ensure t)

;; eye candy
;; 1st time
;; Run M-x nerd-icons-install-fonts
;; Then install those icons on Windows by double clicking and choosing install
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Hide minor modes
(use-package diminish
  :ensure t)

;; A package with a great selection of themes:
;; https://protesilaos.com/emacs/ef-themes
;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (ef-themes-select 'ef-autumn))
(require-theme 'modus-themes)
(load-theme 'modus-vivendi)

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package ace-window
  :ensure t
  :bind("M-p" . ace-window))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-c :" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))
                                        ;(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
                                        ;(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package dired
  :ensure nil

  ;; Dired listing switches; don't work for windows
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables,
  ;;       '/' to directories, etc.
  :config
  (if (string-equal system-type "gnu/linux")
      (setq dired-listing-switches "-aho --group-directories-first"))
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq ls-lisp-dirs-first t)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  ;;(require 'dired-x)
  )

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Better minibuffer completion
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;;; 2023-07-12 Causes problem with vertico--exhibit string-width
;; ;; Enable rich annotations using the Marginalia package
;; (use-package marginalia
;;   ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;;   ;; available in the *Completions* buffer, add it to the
;;   ;; `completion-list-mode-map'.
;;   :bind (:map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))

;;   ;; The :init section is always executed.
;;   :init

;;   ;; Marginalia must be actived in the :init section of use-package such that
;;   ;; the mode gets enabled right away. Note that this forces loading the
;;   ;; package.
;;   (marginalia-mode));; Show lots of useful stuff in the minibuffer
;; (use-package marginalia
;;   :after vertico
;;   :ensure t
;;   :init
;;   (marginalia-mode))

;; https://grtcdr.tn/posts/2023-01-24.html
;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package markdown-mode
  :ensure t)

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "p:/hunspell-1.3.2-3-w32-bin/bin"))
  (setq ispell-program-name "hunspell" ; use hunspell instead of ispell
        ispell-extra-args '("-d en_AU"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (turn-off-auto-fill))

;; https://grtcdr.tn/posts/2023-01-24.html
(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((csharp-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
             '(csharp-mode . ("csharp-ls"))))

(use-package meson-mode
  :ensure t
  :mode "meson.build")

;; GNU Global
;; tags for code navigation
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  )

(defun my-c-mode-hook ()
  "C mode customisations."
  (c-set-style "linux")
  (setq c-basic-offset 4))

(use-package cc-mode
  :hook (c-mode . my-c-mode-hook))

(use-package org
  :mode (("\\.org$" . org-mode)))

;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :ensure t
  :bind("<f5>" . deadgrep))

(use-package yaml-mode
  :ensure t
  :mode "\\.json\\'")

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))
