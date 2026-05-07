;;; emergency-boot.el --- Emergency boot file for Emacs
;;; Commentary:
;; This file will get Emacs running when network issues prevent normal startup

;;; Code:

;; Basic settings
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(column-number-mode)
(global-display-line-numbers-mode t)

;; Make *scratch* empty
(setq initial-scratch-message "")

;; No startup screen
(setq inhibit-startup-message t)

;; No backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Fix PATH for macOS
(setenv "PATH" (concat "/opt/homebrew/bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/opt/homebrew/bin" "/usr/local/bin") exec-path))

;; Load a simple theme
(load-theme 'tango-dark t)

;; Basic keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Recent files
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Show matching parentheses
(show-paren-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

(message "Emergency boot complete! Emacs is running with minimal configuration.")

(provide 'emergency-boot)
;;; emergency-boot.el ends here
