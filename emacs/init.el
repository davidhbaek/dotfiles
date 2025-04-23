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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(tree-sitter-hl-face:jsx-attribute ((t :inherit font-lock-constant-face)))
 '(tree-sitter-hl-face:jsx-element ((t :inherit font-lock-function-name-face :weight bold)))
 '(tree-sitter-hl-face:jsx-expression ((t :inherit font-lock-variable-name-face)))
 '(tree-sitter-hl-face:jsx-text ((t :inherit default)))
 '(typescript-ts-jsx-attribute-face ((t :inherit font-lock-constant-face)))
 '(typescript-ts-jsx-tag-face ((t :inherit font-lock-function-name-face :weight bold)))
 '(typescript-ts-jsx-text-face ((t :inherit default)))
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

;; Set up the tree-sitter load path
(setq treesit-extra-load-path
      (list (expand-file-name "~/.emacs.d/tree-sitter/tree-sitter-javascript")))

;; Enable tree-sitter for JavaScript mode
(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)))

(provide 'init)
;;; init.el ends here
