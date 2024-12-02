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

;; Python Configuration
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

;; General JavaScript/TypeScript settings
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

;; TypeScript configuration with Tide
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq tide-completion-show-source t
        tide-always-show-documentation t))

;; JavaScript configuration with js2-mode
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

;; JSX/TSX configuration with JTSX
(use-package jtsx
  :ensure t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :init
  (add-hook 'jtsx-mode-hook #'jtsx-setup-buffer)
  :hook ((jtsx-jsx-mode . lsp-deferred)
         (jtsx-tsx-mode . lsp-deferred))
  :config
 (setq jtsx-indent-level 2))

(provide 'db-languages)
;;; db-languages.el ends here
