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
  (setq projectile-project-search-path '("~/Desktop/repos"))
  ;; Enable project indexing for faster searches
  (setq projectile-indexing-method 'alien)
  ;; Enable caching for better performance with many projects
  (setq projectile-enable-caching t)
  ;; Auto-discover projects when Emacs starts
  (projectile-discover-projects-in-search-path)
  :custom ((projectile-completion-system 'ivy))  ; Use ivy for completion
  :bind-keymap ("C-c p" . projectile-command-map))

;; Integration between projectile and counsel
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; Add org-roam directory to Projectile's globally ignored directories
(with-eval-after-load 'org-roam
(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories 
               (file-name-nondirectory (directory-file-name org-roam-directory))))
  
  ;; Rebuild Projectile's cache to apply the changes
  (projectile-invalidate-cache nil))

(use-package magit
  :defer
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :ensure t)
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

;; Modern TypeScript/TSX configuration for Emacs 29
;; This uses the built-in tree-sitter support for superior syntax highlighting

;; Define the grammar sources for TypeScript/TSX
;; This tells Emacs where to download the necessary parsers
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

;; Set up file associations for TypeScript and TSX files
;; This ensures .tsx files open in tsx-ts-mode and .ts files in typescript-ts-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; Configure the typescript tree-sitter mode
(with-eval-after-load 'typescript-ts-mode
  ;; Enhance syntax highlighting for JSX/TSX elements
  (setq typescript-ts-mode-indent-offset 2)
  
  ;; Set up custom faces for different TSX elements
  (custom-set-faces
   ;; Make component names stand out
   '(typescript-ts-jsx-tag-face ((t :inherit font-lock-function-name-face :weight bold)))
   ;; Style attributes distinctly
   '(typescript-ts-jsx-attribute-face ((t :inherit font-lock-constant-face)))
   ;; Style JSX text content
   '(typescript-ts-jsx-text-face ((t :inherit default))))

  ;; Add development tools to TSX editing
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              ;; Enable LSP support
              (lsp-deferred)
              ;; Show matching parentheses
              (show-paren-mode 1)
              ;; Enable automatic bracket/quote pairing
              (electric-pair-mode 1)
              ;; Add colorful parentheses
              (rainbow-delimiters-mode 1))))

;; LSP-specific settings for TypeScript/TSX
(with-eval-after-load 'lsp-mode
  (setq lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-preferences-quote-style "single"
        lsp-typescript-format-enable t
        lsp-typescript-format-insert-space-after-comma t
        lsp-typescript-format-insert-space-after-semicolon-in-for-statements t
        ;; ADD THESE LINES:
        lsp-typescript-format-indent-size 2
        lsp-typescript-format-convert-tabs-to-spaces t
        ;; Keep your existing settings below:
        lsp-typescript-suggest-complete-function-calls t
        lsp-typescript-implementations-code-lens-enabled t
        lsp-typescript-references-code-lens-enabled t))

;; Minimal indentation settings for React/TypeScript code
(with-eval-after-load 'typescript-ts-mode
  (setq typescript-ts-mode-indent-offset 2)
  (setq tsx-ts-mode-indent-offset 2)

  ;; Keep your existing faces settings
  (custom-set-faces
   '(typescript-ts-jsx-tag-face ((t :inherit font-lock-function-name-face :weight bold)))
   '(typescript-ts-jsx-attribute-face ((t :inherit font-lock-constant-face)))
   '(typescript-ts-jsx-text-face ((t :inherit default))))

  ;; Update your tsx-ts-mode-hook with format settings
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              (lsp-deferred)
              (show-paren-mode 1)
              (electric-pair-mode 1)
              (rainbow-delimiters-mode 1)
              ;; Make sure format-on-save is using the right settings
              (setq-local lsp-typescript-format-indent-size 2)
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))


;; ADD this new section to ensure indentation is set correctly
(add-hook 'tsx-ts-mode-hook
          (lambda ()
            ;; Explicitly set indentation each time a TSX file is loaded
            (setq-local indent-tabs-mode nil)
            (setq-local typescript-ts-mode-indent-offset 2)
            (setq-local tsx-ts-mode-indent-offset 2)
            (when (fboundp 'lsp-workspace-folders-add)
              (lsp-workspace-folders-add (project-root (project-current))))))

;; Ensure web-mode uses the same indentation style
(use-package web-mode
  :ensure t
  :mode (("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Disable LSP formatting for TypeScript/TSX files
(with-eval-after-load 'lsp-mode
  (setq-default lsp-typescript-format-enable nil))
(provide 'db-development)
;;; db-development.el ends here
