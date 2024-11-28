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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   '(esup gptel zuul zenburn-theme yasnippet yaml-mode which-key typescript-mode tree-sitter-langs todotxt tide rjsx-mode rainbow-delimiters python-mode python-black prettier-js ox-hugo org-tree-slide org-superstar org-roam org-bullets multiple-cursors move-text magit lsp-ui jtsx json-mode jedi ivy-prescient helpful graphql-mode go-mode flycheck-golangci-lint exec-path-from-shell editorconfig dotenv-mode doom-themes doom-modeline dockerfile-mode dired-subtree counsel-projectile company command-log-mode anzu all-the-icons-ivy-rich)))

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
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

(provide 'init)
;;; init.el ends here
