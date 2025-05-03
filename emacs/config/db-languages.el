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



;; ====================================
;; Go Configuration
;; ====================================

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


;; ====================================
;; Python Configuration
;; ====================================

;; We're using LSP mode for consistency with your other language configurations
(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  :config
  ;; Similar to your Go configuration, we'll add electric pair mode
  (add-hook 'python-mode-hook
            (lambda ()
              (electric-pair-mode 1)
              (show-paren-mode 1))))

;; LSP Python configuration
;; This integrates with your existing LSP setup while adding Python-specific settings
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred)))
  :custom
  (lsp-pyright-typechecking-mode "basic"))

;; Python-specific LSP settings
;; These complement your existing LSP configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-python-ms-auto-install-server t)
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer t t)
              (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package python-isort
  :ensure t
  :after python
  :hook (python-mode . python-isort-on-save-mode))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; Python-specific configuration
  (sp-local-pair 'python-mode "'" "'")
  (sp-local-pair 'python-mode "\"" "\"")
  (sp-local-pair 'python-mode "(" ")")
  (sp-local-pair 'python-mode "[" "]")
  (sp-local-pair 'python-mode "{" "}"))

; =======================================
;; TypeScript and JavaScript Configuration
;; =======================================

;; Core TypeScript settings
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  ;; Basic indentation and formatting
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 2)
  ;; Enable JSX parsing in TypeScript
  (setq typescript-enabled-frameworks '(typescript jsx))
  ;; Custom indentation rules
  (setq typescript-indent-switch-clauses t))

;; TypeScript LSP configurations
(with-eval-after-load 'lsp-mode
  ;; Import and formatting preferences
  (setq lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-preferences-quote-style "single"
        lsp-typescript-format-enable t
        lsp-typescript-format-insert-space-after-comma t
        lsp-typescript-format-insert-space-after-semicolon-in-for-statements t
        lsp-typescript-surveys-enabled nil)
  
  ;; Enhanced TypeScript features
  (setq lsp-typescript-suggest-complete-function-calls t
        lsp-typescript-implementations-code-lens-enabled t
        lsp-typescript-references-code-lens-enabled t
        lsp-typescript-organize-imports-on-save t)
  
  ;; Performance optimizations for TypeScript
  (setq lsp-typescript-max-ts-server-memory 3072
        lsp-file-watch-threshold 5000))

;; JSX/TSX configuration
(use-package jtsx
  :ensure t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :init
  (add-hook 'jtsx-mode-hook #'jtsx-setup-buffer)
  :config
  (setq jtsx-indent-level 2
        jtsx-jsx-syntax-highlighting-mode t
        jtsx-enable-jsx-electric-closing-element t
        jtsx-enable-electric-keys t)
  
  ;; Enhance JSX component visibility
  (face-spec-set 'jtsx-jsx-component-face
                 '((t :inherit font-lock-function-name-face :weight bold)))
  
  ;; Improved tag matching
  (setq jtsx-match-tag-style 'highlight))

;; JavaScript configuration
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil
        js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t)
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-global-externs '("event" "api" "axios"))
              (setq js2-include-node-externs t))))

;; Global JavaScript/TypeScript settings
(setq-default js-indent-level 2
              indent-tabs-mode nil)


(provide 'db-languages)
;;; db-languages.el ends here
