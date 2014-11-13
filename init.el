;;; package --- Summary
; Jonno's linux emacs

;;; Commentary:

;;; Code:

;; Add the user-contributed repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; installed packages
;; ac-js2             20131112.403 installed  Auto-complete source for Js2-mode, with navigation
;; angular-snippets   20131125.314 installed  Yasnippets for AngularJS
;; auto-complete      20140127.632 installed  Auto Completion for GNU Emacs
;; dash               20140121.427 installed  A modern list library for Emacs
;; epl                20140102.211 installed  Emacs Package Library
;; f                  20140128.... installed  Modern API for working with files and directories
;; flycheck           20140130.... installed  On-the-fly syntax checking (Flymake done right)
;; js2-mode           20140114     installed  Improved JavaScript editing mode
;; multiple-cursors   20140105.259 installed  Multiple cursors for Emacs.
;; pkg-info           20131101.408 installed  Information about packages
;; popup              20140124.... installed  Visual Popup User Interface
;; s                  20131223.944 installed  The long lost Emacs string manipulation library.
;; simple-httpd       20140123.... installed  pure elisp HTTP server
;; skewer-mode        20140123.... installed  live browser JavaScript, CSS, and HTML interaction
;; smartparens        20140222.826 installed  Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
;; yasnippet          20140106.... installed  Yet another snippet extension for Emacs.

(setq make-backup-files nil)

;; theme
(load-theme 'wombat)

(setq-default ispell-program-name "aspell")

;; display time and date
(setq display-time-day-and-date 't)
(display-time)

(setq frame-title-format "%b")
(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28))

;; replace the old buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle window split; switch from horizontal split to vertical split and vice-versa
(defun toggle-window-split ()
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

;; S-left etc to move among windows
(windmove-default-keybindings)

;; Don't wrap long lines.
(set-default 'truncate-lines t)

;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; smartparens
(smartparens-global-mode t)
(require 'smartparens-config)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ido 
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 
(setq ido-use-filename-at-point 'guess) 

;; smex
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq auto-mode-alist (append '(("\\.include$" . nxml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.config$" . nxml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '( ("\\.build$" . nxml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '( ("\\.aspx$" . nxml-mode)) auto-mode-alist))

(add-hook 'nxml-mode-hook 'turn-off-auto-fill)
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)

;; javascript-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

;;; .emacs (don't put in (require 'csharp-mode))
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;(global-set-key "%" 'match-paren)

;(defun match-paren (arg)
;  "Go to the matching paren if on a paren; otherwise insert %."
; (interactive "p")
; (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;		(t (self-insert-command (or arg 1)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(canlock-password "ed58b26d973cc55ed62a41231cecab932ba6c707")
 '(case-fold-search t)
 '(display-time-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(line-number-display-limit-width 2000)
 '(nxhtml-load t)
 '(ourcomments-ido-ctrl-tab t)
 '(ps-paper-type "a4")
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((Mode . Emacs-Lisp))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(put 'scroll-left 'disabled nil)

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
