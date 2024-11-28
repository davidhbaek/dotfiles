;;; init.el --- Personal Emacs configuration for development workflow

;;; Commentary:
;; A comprehensive Emacs configuration focusing on:
;; - Software development (Go, TypeScript, Python)
;; - Project management and version control
;; - Note-taking and documentation with Org mode
;; - Modern UI and completion frameworks

;;; Code:

;; ====================================
;; Basic UI Settings
;; ====================================

;; Remove visual clutter
(setq inhibit-startup-message t)  ; No splash screen
(toggle-scroll-bar -1)            ; No scroll bars
(tool-bar-mode -1)               ; No toolbar
(tooltip-mode -1)                ; No tooltips
(set-fringe-mode 10)             ; Add some padding on the sides
(menu-bar-mode -1)               ; No menu bar

;; Basic display settings
(column-number-mode)                    ; Show column number in mode line
(global-display-line-numbers-mode t)    ; Show line numbers
(set-face-attribute 'default nil :height 180)  ; Larger font size

;; Editor defaults
(setq-default tab-width 4)              ; 4 spaces per tab
(setq-default js-indent-level 2)        ; 2 space indent for JS
(setq-default typescript-indent-level 2) ; 2 space indent for TS

;; File handling and history
(recentf-mode 1)                        ; Track recently opened files
(setq history-length 25)
(savehist-mode 1)                       ; Save minibuffer history
(save-place-mode 1)                     ; Remember cursor position in files
(global-auto-revert-mode 1)             ; Auto-reload files when changed
(setq global-auto-revert-non-file-buffers t)
(setq use-dialog-box nil)               ; No GUI dialogs
(setq backup-directory-alist `(("." . "~/.saves")))  ; Centralize backups

;; macOS specific settings
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; ====================================
;; Font and Face Configuration
;; ====================================

;; Default faces and themes
(custom-set-faces
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

;; ====================================
;; Package Management
;; ====================================

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ====================================
;; Theme and UI Enhancements
;; ====================================

;; Set up custom theme path and load zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Modern mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))  ; Adjust modeline height

(use-package doom-themes)  ; Additional themes collection

;; Rainbow delimiters for better bracket/paren visualization
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Display available key bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))  ; Show key hints after 0.3 seconds

;; ====================================
;; Navigation and Completion
;; ====================================

;; Ivy - Completion framework
(use-package ivy
  :diminish  ; Don't show mode in modeline
  :bind (("C-s" . swiper)  ; Better search
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))  ; Enable ivy globally

;; Counsel - Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)         ; Enhanced M-x
         ("C-x b" . counsel-ibuffer)    ; Enhanced buffer switching
         ("C-x C-f" . counsel-find-file)  ; Enhanced file opening
         ("C-x r j" . counsel-register)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ; Don't start searches with ^

;; Enhanced minibuffer documentation
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Icons in ivy
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

;; Better help system
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Better sorting for ivy
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (setq prescient-sort-length-enable nil
        ivy-prescient-retain-classic-highlighting t
        prescient-filter-method '(literal initialism regexp)))

;; Better search and replace feedback
(global-anzu-mode +1)
(anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; ====================================
;; Development Tools
;; ====================================

;; Project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))  ; Use ivy for completion
  :bind-keymap ("C-c p" . projectile-command-map))

;; Integration between projectile and counsel
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Git integration
(use-package magit
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

;; ====================================
;; Markdown Configuration
;; ====================================

;; Set pandoc as the markdown processor
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

;; Function to convert markdown to HTML with strapdown.js
(defun markdown-html (buffer)
  "Convert markdown BUFFER content to HTML using strapdown.js for styling."
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" 
            (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

;; Function to create a styled HTML preview of markdown content
(defun my-markdown-filter (buffer)
  "Apply GitHub-style CSS to markdown preview of BUFFER content."
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

;; Function to preview markdown in real-time
(defun my-markdown-preview ()
  "Start a live preview server for the current markdown buffer."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

;; ====================================
;; File Management (Dired)
;; ====================================

;; Basic dired configuration
(setq insert-directory-program "gls" dired-use-ls-dired t)

;; Main dired configuration
(use-package dired
  :ensure nil  ; Built into Emacs
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))  ; Quick jump to current file's directory
  :custom ((dired-listing-switches "-lagho --group-directories-first"))  ; Better listing format
  :config
  ;; Custom navigation keys in dired
  (bind-key "C-p" #'dired-up-directory dired-mode-map)        ; Go up directory with C-p
  (bind-key "C-n" #'dired-find-file dired-mode-map)          ; Open file/dir with C-n
  (bind-key "M-n" #'dired-find-file-other-window dired-mode-map))  ; Open in new window

;; Enhanced directory tree functionality
(use-package dired-subtree
  :after dired
  :config
  ;; Toggle subtree view with TAB
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; macOS specific dired configuration
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))  ; Fix for macOS ls command

;; ====================================
;; Org Mode Configuration
;; ====================================

;; Basic org-mode setup function
(defun db/org-mode-setup()
  "Configure basic org-mode settings and appearance."
  (org-indent-mode)              ; Enable indentation
  (variable-pitch-mode 1)        ; Use variable-pitch fonts
  (auto-fill-mode 0)             ; Disable auto-fill
  (visual-line-mode 1))          ; Enable visual line mode

;; Main org-mode configuration
(use-package org
  :hook (org-mode . db/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")           ; Use this symbol for collapsed sections
  (setq org-hide-emphasis-markers t)  ; Hide markup symbols
  (setq org-src-tab-acts-natively t)) ; Better tab behavior in source blocks

;; Custom bullet appearance for lists
(font-lock-add-keywords 'org-mode
                       '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Configure org todo states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "QA" "|" "DONE")))

;; Enable inline images by default
(setq org-startup-with-inline-images t)

;; Org face configuration for better typography
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (headline           `(:inherit default)))
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; Better bullet points in org-mode
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-roam configuration for note-taking
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org-roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Presentation mode configuration
(defun efs/presentation-setup()
  "Setup environment for org-mode presentations."
  (setq text-scale-mode-amount 2)
  (org-display-inline-images)
  (text-scale-mode 1))

(defun efs/presentation-end()
  "Cleanup after org-mode presentations."
  (text-scale-mode 0))

;; Presentation tool configuration
(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation start")
  (org-tree-slide-deactive-message "Presentation end")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // "))

;; ====================================
;; Custom Functions
;; ====================================

;; Navigation in mark ring
(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Useful for navigating back to previous positions."
  (interactive)
  (set-mark-command t))

;; Enhanced mark ring navigation
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty.
Allows for navigating forward in mark history."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))

;; Smart paired character insertion
(defun electric-pair ()
  "Insert character pair without surrounding spaces at end of line.
Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair))
    (self-insert-command 1)))

;; ====================================
;; Key Bindings
;; ====================================

;; Move text up/down
(use-package move-text
  :ensure t)
(move-text-default-bindings)  ; M-up/M-down to move lines/regions

;; Multiple cursors for simultaneous editing
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)         ; Mark next occurrence
   ("C-<" . mc/mark-previous-like-this)     ; Mark previous occurrence
   ("C-?" . mc/mark-all-like-this)          ; Mark all occurrences
   ("C-S-c C-S-c" . mc/edit-lines)))        ; Add cursor to each line in region

;; Custom global key bindings
(global-set-key (kbd "C-w") 'kill-ring-save)  ; Copy (different from default)
(global-set-key (kbd "M-w") 'kill-region)     ; Cut (different from default)
(global-set-key (kbd "C-j") 'pop-global-mark) ; Jump to previous mark
(global-set-key (kbd "C-k") 'unpop-to-mark-command)  ; Jump forward in mark ring

;; Window movement
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))  ; Use Shift+arrows to move between windows

;; ====================================
;; Environment and PATH
;; ====================================

;; Quick access to important files via registers
(setq dotfiles-directory "~/.dotfiles/")
(set-register ?i (cons 'file (concat dotfiles-directory "init.el")))    ; Jump to init.el with C-x r j i
(set-register ?w (cons 'file (concat dotfiles-directory "scripts/watch.sh")))  ; Jump to watch.sh with C-x r j w

;; Environment setup for development tools
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Set up Go development environment
(setenv "GOPATH" "/Users/david.baek/go")

;; Configure system PATH
(setenv "PATH" "/Users/david.baek/.cabal/bin:/Users/david.baek/.ghcup/bin:/Users/david.baek/google-cloud-sdk/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Users/david.baek/go/bin:/Users/david.baek/go/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/david.baek/go/bin:/usr/local/go/bin")

;; Provide this file as a feature
(provide 'init)
;;; init.el ends here
