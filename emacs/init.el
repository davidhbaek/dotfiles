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
(require 'db-jira)
(require 'db-terminal)
(require 'db-fonts)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   '(lua-mode terraform-mode python-isort web-mode markdown-ts-mode 0blayout 0x0 markdown-mode markdown-preview-eww markdown-preview-mode grip-mode treesit-auto lsp-pyright org-roam-ui org-jira esup gptel zuul zenburn-theme yasnippet yaml-mode which-key typescript-mode tree-sitter-langs todotxt tide rjsx-mode rainbow-delimiters python-mode python-black prettier-js ox-hugo org-tree-slide org-superstar org-roam org-bullets multiple-cursors move-text magit lsp-ui jtsx json-mode jedi ivy-prescient helpful graphql-mode go-mode flycheck-golangci-lint exec-path-from-shell editorconfig dotenv-mode doom-themes doom-modeline dockerfile-mode dired-subtree counsel-projectile company command-log-mode anzu all-the-icons-ivy-rich)))

;; Set up the tree-sitter load path
(setq treesit-extra-load-path
      (list (expand-file-name "~/.emacs.d/tree-sitter/tree-sitter-javascript")))

;; Enable tree-sitter for JavaScript mode
(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)))

(provide 'init)
;;; init.el ends here
