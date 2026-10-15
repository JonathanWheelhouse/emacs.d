;;; init.el -- Jono's Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Jonathan Wheelhouse

;; Author: Jonathan Wheelhouse <jonathan.wheelhouse@gmail.com>
;; URL: https://github.com/JonathanWheelhouse/emacs-config

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; ------------------------------------------------------------
;; Performance tweaks for modern machines
;; ------------------------------------------------------------
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb
(setq load-prefer-newer t)

;; ------------------------------------------------------------
;; Custom settings file (must be loaded!)
;; ------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

;; ------------------------------------------------------------
;; Edit / reload configuration
;; ------------------------------------------------------------
(defun config-visit ()
  "Edit config."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c e") #'config-visit)

(defun config-reload ()
  "Reload config at runtime."
  (interactive)
  (when (file-readable-p "~/.emacs.d/init.el")
    (load-file (expand-file-name "~/.emacs.d/init.el"))))

(global-set-key (kbd "C-c r") #'config-reload)

;; ------------------------------------------------------------
;; Windows-specific config
;; ------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (let ((git-bin "C:/Program Files/Git/bin")
        (git-usr-bin "C:/Program Files/Git/usr/bin"))
    (add-to-list 'exec-path git-bin)
    (add-to-list 'exec-path git-usr-bin)
    (setenv "PATH"
            (concat git-bin ";" git-usr-bin ";" (getenv "PATH")))))

;; ------------------------------------------------------------
;; Fonts
;; ------------------------------------------------------------
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Consolas" :height 120))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "Inconsolata" :height 220)))

;; ------------------------------------------------------------
;; Package setup
;; ------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)

;; ------------------------------------------------------------
;; use-package behaviour
;; ------------------------------------------------------------
;;(setq use-package-always-ensure t)
;;(setq use-package-expand-minimally t)

;; Helps avoid TLS/signature issues in corp environments
(setq package-check-signature nil)

;; ------------------------------------------------------------
;; Package updates (manual only)
;; ------------------------------------------------------------
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-now))

;; Run manually:
;; M-x auto-package-update-now

;; ------------------------------------------------------------
;; Core behaviour
;; ------------------------------------------------------------
(setq make-backup-files nil)
(setq frame-title-format "%b")
(setq inhibit-startup-screen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(column-number-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package savehist
  :init
  (savehist-mode))

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)

(setq require-final-newline t)

(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

;; ------------------------------------------------------------
;; Appearance
;; ------------------------------------------------------------
;; 1st time
;; M-x all-the-icons-install-fonts
;; Then install those icons on Windows by double clicking and choosing install
(use-package all-the-icons :ensure t)

;; 1st time
;; Run M-x nerd-icons-install-fonts
;; Then install those icons on Windows by double clicking and choosing install
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(load-theme 'modus-vivendi t)

;; ------------------------------------------------------------
;; Navigation / UI
;; ------------------------------------------------------------
;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :config (windmove-default-keybindings))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-c :" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package vundo :ensure t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package diminish :ensure t)

;; ------------------------------------------------------------
;; Editing helpers
;; ------------------------------------------------------------
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(show-paren-mode +1)

(use-package rainbow-delimiters :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package whitespace
  :hook ((prog-mode text-mode) . whitespace-mode)
  :hook (before-save . whitespace-cleanup)
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

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line))

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; ------------------------------------------------------------
;; Evil
;; ------------------------------------------------------------
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; ------------------------------------------------------------
;; Buffers / files
;; ------------------------------------------------------------
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package dired
  :ensure nil
  :config
  (if (eq system-type 'gnu/linux)
      (setq dired-listing-switches "-aho --group-directories-first")
    ;; Cross-platform fix for Windows Dired sorting
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil)
    (setq ls-lisp-dirs-first t))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; ------------------------------------------------------------
;; Completion
;; ------------------------------------------------------------
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)

(use-package vertico
  :ensure t
  :config (vertico-mode 1))

(use-package marginalia
  :ensure t                       ; Ensure it downloads on new setups
  :init (marginalia-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :bind
  ;; Fixes the conflict between Corfu popup selections and Evil mode
  (:map corfu-map
        ("RET" . nil)             ; Let Enter do its normal job
        ("<tab>" . corfu-complete) ; Use Tab to complete inside popup
        ("C-n" . corfu-next)      ; Standard vim/emacs dropdown navigation
        ("C-p" . corfu-previous)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)))

(setq consult-narrow-key "<")

;; ------------------------------------------------------------
;; Languages
;; ------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :hook (nxml-mode . turn-off-auto-fill))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((csharp-mode . eglot-ensure)
         (csharp-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((csharp-mode csharp-ts-mode) . ("csharp-ls"))))

(use-package treesit-auto
  :demand t
  :custom
  ;; Prompt for confirmation before downloading a grammar
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package meson-mode
  :ensure t
  :mode ("meson\\.build\\'" . meson-mode))

;; ------------------------------------------------------------
;; Misc tools
;; ------------------------------------------------------------
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)))   ; Added the most popular global magit binding

(use-package deadgrep
  :ensure t
  :bind ("<f5>" . deadgrep))

(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode java-mode) . ggtags-mode))

;; Integrated cleanly into Emacs built-in C engine setup
(use-package cc-mode
  :ensure nil
  :hook (c-mode . (lambda ()
                    (c-set-style "linux")
                    (setq c-basic-offset 4))))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; ------------------------------------------------------------
;; Org
;; ------------------------------------------------------------
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode))

;;; init.el ends here
