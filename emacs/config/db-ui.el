;;; db-ui.el --- UI configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Basic UI Settings
;; ====================================

;; Remove visual clutter
(setq inhibit-startup-message t)  ; No splash screen
(toggle-scroll-bar -1)            ; No scroll bars
(tool-bar-mode -1)               ; No toolbar
(tooltip-mode -1)                ; No tooltips
(set-fringe-mode 10)             ; Add some padding on the sides
(menu-bar-mode -1)               ; No menu bar

;; Basic display settings
(column-number-mode)                    ; Show column number in mode line
(global-display-line-numbers-mode t)    ; Show line numbers
(set-face-attribute 'default nil :height 180)  ; Larger font size

;; Editor defaults
(setq-default tab-width 4)              ; 4 spaces per tab
(setq-default js-indent-level 2)        ; 2 space indent for JS
(setq-default typescript-indent-level 2) ; 2 space indent for TS

;; File handling and history
(recentf-mode 1)                        ; Track recently opened files
(setq history-length 25)
(savehist-mode 1)                       ; Save minibuffer history
(save-place-mode 1)                     ; Remember cursor position in files
(global-auto-revert-mode 1)             ; Auto-reload files when changed
(setq global-auto-revert-non-file-buffers t)
(setq use-dialog-box nil)               ; No GUI dialogs
(setq backup-directory-alist `(("." . "~/.saves")))  ; Centralize backups

;; macOS specific settings
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; ====================================
;; Font and Face Configuration
;; ====================================

;; Default faces and themes
(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 160 :width normal :foundry "nil" :family "Menlo"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(org-document-title ((t (:inherit default :font "Lucida Grande" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :font "Lucida Grande" :height 1.5))))
 '(org-level-2 ((t (:inherit default :font "Lucida Grande" :height 1.3))))
 '(org-level-3 ((t (:inherit default :font "Lucida Grande" :height 1.1))))
 '(org-level-4 ((t (:inherit default :font "Lucida Grande"))))
 '(org-level-5 ((t (:inherit default :font "Lucida Grande"))))
 '(org-level-6 ((t (:inherit default :font "Lucida Grande"))))
 '(org-level-7 ((t (:inherit default :font "Lucida Grande"))))
 '(org-level-8 ((t (:inherit default :font "Lucida Grande"))))
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

(provide 'db-ui)
;;; db-ui.el ends here
