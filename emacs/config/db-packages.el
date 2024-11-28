;;; db-packages.el --- Packages configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Package Management
;; ====================================

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ====================================
;; Theme and UI Enhancements
;; ====================================

;; Set up custom theme path and load zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Modern mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))  ; Adjust modeline height

(use-package doom-themes)  ; Additional themes collection

;; Rainbow delimiters for better bracket/paren visualization
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Display available key bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))  ; Show key hints after 0.3 seconds

(provide 'db-packages)
;;; db-packages.el ends here

