;;; init.el -- Jono's Emacs configuration

;; Copyright (c) 2020 Jonathan Wheelhouse

;; Author: Jonathan Wheelhouse <jonathan.wheelhouse@gmail.com>
;; URL: https://github.com/JonathanWheelhouse/emacs-config

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

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

;; set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Inconsolata" (font-family-list))
                                        ;    (set-frame-font "Inconsolata" t t))))
    (set-frame-font "Fira Code" t t))))

;; Set up package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 27)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(scroll-bar-mode nil)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(savehist-mode 1)

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; tabs
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Newline at end of file
(setq require-final-newline t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; eye candy
;; 1st time, after all the icons, run
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(defun toggle-window-split ()
  "Toggle window split; switch from horizontal split to vertical split and vice-versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'toggle-window-split)

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))
(with-eval-after-load "use-package"
  (bind-key "C-x C-d" #'duplicate-line))

;; Don't wrap long lines.
(set-default 'truncate-lines t)

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

;; Hide minor modes
(use-package diminish
  :ensure t)

;; theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))
; (load-theme 'wombat)

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

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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

(use-package evil
  :ensure t
  :commands evil-mode)

(global-set-key (kbd "C-z") 'evil-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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
  (require 'dired-x))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

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

;; completion framework
;; counsel and swiper depend on it
(use-package ivy
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :config (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-ibuffer)
         ("M-y"     . counsel-yank-pop)))

(use-package swiper
  :after ivy
  :ensure t
  :config
  (progn
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    ))

(use-package smex
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

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

;; lsp & omnisharp
(defun efs/lsp-mode-setup ()
  "Customise lsp mode for csharp."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ivy
  :ensure t)

(use-package csharp-mode
  :ensure t
  :init (add-hook 'csharp-mode-hook 'lsp-deferred)
  :mode "\\.cs")

(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

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

;;; init.el ends here
