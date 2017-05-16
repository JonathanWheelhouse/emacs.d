;;; init.el -- Jono's Emacs configuration

;; Copyright (c) 2016 Jonathan Wheelhouse

;; Author: Jonathan Wheelhouse <jonathan.wheelhouse@gmail.com>
;; URL: https://github.com/JonathanWheelhouse/emacs-config

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; windows only stuff
(when (string-equal system-type "windows-nt")
  (defvar git-bin)
  (setq git-bin "C:\\Program Files\\Git\\usr\\bin")
  (setenv "PATH" (concat git-bin ";" (getenv "PATH")))
  (setq exec-path (append '(git-bin) exec-path)))

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


(if (string-equal system-type "windows-nt")
    (set-frame-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
  (set-frame-font "Inconsolata-16"))

;; Set up package sources
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq make-backup-files nil)

(setq frame-title-format "%b")

;; disable startup screen
(setq inhibit-startup-screen t)

;; display time and date
(setq display-time-day-and-date 't)
(display-time-mode t)

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

(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
;;; never use tabs for alignment
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

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

;; replace the old buffer menu
(global-set-key (kbd "C-x C-b") #'ibuffer)

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

;; theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))
  ;(load-theme 'wombat)

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-c :" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config))
;(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package paren
  :config
  (show-paren-mode +1))

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

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

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
  :ensure t
  :diminish rainbow-delimiters-mode
)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    ))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package markdown-mode
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
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

(use-package visual-regexp
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package csharp-mode
  :ensure t
  :mode "\\.cs'")

;; GNU Global
;; tags for code navigation
(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )

(use-package org
  :mode (("\\.org$" . org-mode)))

;; apt install silversearcher-ag
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ggtags abbrev counsel swiper ag csharp-mode visual-regexp json-mode js2-mode which-key aggressive-indent flycheck imenu-anywhere zop-to-char company markdown-mode smex ido-ubiquitous rainbow-mode rainbow-delimiters move-text anzu multiple-cursors smartparens expand-region projectile magit avy material-theme use-package)))
 '(send-mail-function (quote mailclient-send-it)))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
