;;; db-languages.el --- Programming languages configurations

;;; Commentary:
;; Configurations for programming languages:
;; - Go
;; - JavaScript/TypeScript
;; - Python

;;; Code:


;; ====================================
;; Programming Languages
;; ====================================

;; Go configuration
(setq lsp-enable-file-watchers nil)
(setq gofmt-command "goimports")
(setq lsp-go-use-gofumpt t)

;; Go save hooks
(defun lsp-go-install-save-hooks ()
  "Set up before-save hooks for Go development."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Go mode settings
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; Electric pair mode for Go
(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map "\"" 'electric-pair)
            (define-key go-mode-map "\'" 'electric-pair)
            (define-key go-mode-map "(" 'electric-pair)
            (define-key go-mode-map "[" 'electric-pair)
            (define-key go-mode-map "{" 'electric-pair)))

;; TypeScript/JavaScript configuration
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; TypeScript Interactive Development Environment
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; JSX support
(use-package jtsx 
  :mode "\\.jsx?\\'"
  :hook (jtsx-mode . lsp-deferred))

;; Python configuration
(use-package jedi
  :ensure t
  :hook (python-mode . jedi:setup)
  :init
  (setq jedi:complete-on-dot t)  ; Start completion after typing .
  :config
  ;; Electric pair configuration for Python
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map "\"" 'electric-pair)
              (define-key python-mode-map "\'" 'electric-pair)
              (define-key python-mode-map "(" 'electric-pair)
              (define-key python-mode-map "[" 'electric-pair)
              (define-key python-mode-map "{" 'electric-pair))))

;; Additional Python enhancements
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

;; JavaScript additional settings
(setq-default js-indent-level 2)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun delete-tern-process ()
  "Kill the tern server if it is running."
  (interactive)
  (delete-process "Tern"))

(provide 'db-languages)
;;; db-languages.el ends here
