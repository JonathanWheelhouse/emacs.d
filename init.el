;;; Commentary:

;;; Code

;; Add the user-contributed repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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

;; iswitch mode
(iswitchb-mode)

;; S-left etc to move among windows
(windmove-default-keybindings)

;; Don't wrap long lines.
(set-default 'truncate-lines t)

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
