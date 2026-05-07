;;; minimal-init.el --- Minimal initialization to fix issues

;;; Commentary:
;; This is a minimal configuration to fix the current issues

;;; Code:

;; Basic package setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fix the f-glob shortdoc issue by patching the shortdoc library
(with-eval-after-load 'shortdoc
  (defun shortdoc-function-arglist-junk-p (arglist)
    "Return non-nil if ARGLIST has &key :noeval or &optional :noeval."
    (or (and (memq '&key arglist)
             (memq :noeval (cdr (memq '&key arglist))))
        (and (memq '&optional arglist)
             (memq :noeval (cdr (memq '&optional arglist)))))))

;; Fix PATH for subprocesses
(setenv "PATH" (concat "/opt/homebrew/bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/opt/homebrew/bin" "/usr/local/bin") exec-path))

;; Load exec-path-from-shell to get environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Verify git is available
(use-package magit
  :ensure t
  :config
  (message "Git is available: %s" (executable-find "git")))

;; Display useful information
(message "Shell PATH: %s" (getenv "PATH"))
(message "Exec path: %s" exec-path)
(message "Git location: %s" (executable-find "git"))

(provide 'minimal-init)
;;; minimal-init.el ends here
