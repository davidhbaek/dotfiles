;;; db-development.el --- Configurations for development tools

;;; Commentary:
;; Configurations for development tools like:
;; - Project managemant (projectile)
;; - Version control (magit)
;; - IDE (lsp-mode)
;; - Syntax checkers (flycheck)
;; - Code completion (company)
;; - Code snippets (yasnippet)

;;; Code:

;; ====================================
;; Development Tools
;; ====================================

;; Project management
(use-package projectile
  :defer
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))  ; Use ivy for completion
  :bind-keymap ("C-c p" . projectile-command-map))

;; Integration between projectile and counsel
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Git integration
(use-package magit
  :defer
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; GitHub pull request integration
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url))

;; LSP (Language Server Protocol) support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :defer
  :init
  (setq lsp-keymap-prefix "C-c l")  ; Set LSP keybinding prefix
  :ensure t
  :hook ((go-mode . lsp-deferred)   ; Enable LSP for Go
         (python-mode . lsp-deferred) ; Enable LSP for Python
         (typescript-mode . lsp-deferred)) ; Enable LSP for TypeScript
  :config
  (lsp-enable-which-key-integration t))

;; Additional LSP configuration for file watching
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.opt\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.go$"))

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;; Code completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0          ; No delay in showing suggestions
        company-minimum-prefix-length 1  ; Show suggestions after single character
        company-tooltip-align-annotations t)  ; Align annotations to the right
  :bind (:map company-active-map
              ("C-k" . company-select-previous-or-abort)
              ("C-j" . company-select-next-or-abort)))

;; Snippet system
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; EditorConfig support
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Syntax highlighting improvements
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(use-package tree-sitter
  :config
  (use-package tree-sitter-langs
    :config
    (add-to-list 'tree-sitter-major-mode-language-alist '(dockerfile-mode . dockerfile))))

(provide 'db-development)
;;; db-development.el ends here
