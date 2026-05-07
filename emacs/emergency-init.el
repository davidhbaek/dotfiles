;;; emergency-init.el --- Emergency minimal init file

;;; Commentary:
;; Use this when your main init file isn't working

;;; Code:

;; Basic settings
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Package system setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fix PATH issues
(setenv "PATH" (concat "/opt/homebrew/bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/opt/homebrew/bin" "/usr/local/bin") exec-path))

;; Basic theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Show key bindings
(use-package which-key
  :config
  (which-key-mode))

;; Fix shortdoc issues with f library
(with-eval-after-load 'shortdoc
  (defun shortdoc-function-arglist-junk-p (arglist)
    "Return non-nil if ARGLIST has &key :noeval or &optional :noeval."
    (or (and (memq '&key arglist)
             (or (memq :noeval (cdr (memq '&key arglist)))
                 (memq ':noeval* (cdr (memq '&key arglist)))))
        (and (memq '&optional arglist)
             (or (memq :noeval (cdr (memq '&optional arglist)))
                 (memq ':noeval* (cdr (memq '&optional arglist))))))))

;; Basic editing modes
(use-package magit
  :ensure t)

;; After this is working, uncomment more packages as needed
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(provide 'emergency-init)
;;; emergency-init.el ends here
