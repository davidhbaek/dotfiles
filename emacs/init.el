;;; init.el --- Personal Emacs configuration for development workflow

;;; Commentary:
;; A comprehensive Emacs configuration focusing on:
;; - Software development (Go, TypeScript, Python)
;; - Project management and version control
;; - Note-taking and documentation with Org mode
;; - Modern UI and completion frameworks

;;; Code:


;; Load configuration files
(add-to-list 'load-path (expand-file-name "config" (file-name-directory load-file-name)))

(require 'db-ui)
(require 'db-packages)
(require 'db-navigation)
(require 'db-development)
(require 'db-languages)
(require 'db-markdown)
(require 'db-file-management)
(require 'db-org)
(require 'db-functions)
(require 'db-keybindings)
(require 'db-env)

(provide 'init)
;;; init.el ends here
