;;; init.el --- Personal Emacs configuration for development workflow

;;; Commentary:
;; A comprehensive Emacs configuration focusing on:
;; - Software development (Go, TypeScript, Python)
;; - Project management and version control
;; - Note-taking and documentation with Org mode
;; - Modern UI and completion frameworks

;;; Code:

;; Move custom variables and faces to a separate file
(setq custom-file (expand-file-name "custom.el" (file-name-directory load-file-name)))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load configuration files
(add-to-list 'load-path (expand-file-name "config" (file-name-directory load-file-name)))

;; Load UI and package configurations first
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
(require 'db-jira)
(require 'db-terminal)
(require 'db-copilot)
(require 'db-fonts)
(require 'db-gptel)
(require 'db-mcp)

;; Set up the tree-sitter load path
(setq treesit-extra-load-path
      (list (expand-file-name "~/.emacs.d/tree-sitter/tree-sitter-javascript")))

;; Enable tree-sitter for JavaScript mode
(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)))

(provide 'init)
;;; init.el ends here
