(setq inhibit-startup-message t)
(toggle-scroll-bar -1) ; Disable scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room

(menu-bar-mode -1)     ; Disable the menu bar

;; Search through recently opened files
(recentf-mode 1)

;; Save the history of inputs to the minibuffer
(setq history-length 25)
(savehist-mode 1)

;; Remember the last place you opened a file
(save-place-mode 1)


;; Don't use the pop up box
(setq use-dialog-box nil)

;; Revert buffers when the file changes on disk
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers 1)

(set-face-attribute 'default nil :height 180)


(setenv "GOPATH" "/Users/david.baek/go")
;; (setenv "PATH" "/Users/david.baek/go/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
(setenv "PATH" "/Users/david.baek/.cabal/bin:/Users/david.baek/.ghcup/bin:/Users/david.baek/google-cloud-sdk/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Users/david.baek/go/bin:/Users/david.baek/go/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/david.baek/go/bin:/usr/local/go/bin")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
 ("org" . "https://orgmode.org/elpa/")
 ("elpa" . "https://elpa.gnu.org/packages/")))



;; Pick a theme to use here
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for org mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Allow for multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-agenda-files '("~/Desktop/life/trips.org"))
 '(package-selected-packages
   '(editorconfig prettier-js zuul jtsx todotxt dockerfile-mode tree-sitter-langs tree-sitter dotenv-mode python-black rustic rust-mode tern xref-js2 spacemacs-theme js2-refactor syslog-mode anzua rjsx-mode web-mode tide lsp-tailwindcss ox-hugo js2-mode typescript-mode org-jira haskell-mode avy anzu flycheck-golangci-lint flycheck org-roam ivy-prescient dired-subtree company-terraform grip-mode exec-path-from-shell pandoc markdownfmt markdown-preview-mode impatient-mode jedi lsp-python-ms json-mode org-tree-slide magit-gh-pulls terraform-mode graphql-mode org-superstar yaml-mode move-text org-bullets multiple-cursors company yasnippet go-mode zenburn-theme which-key use-package rainbow-delimiters magit lsp-ui lsp-ivy helpful doom-themes doom-modeline counsel-projectile command-log-mode all-the-icons-ivy-rich)))
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
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

  (add-hook 'org-mode-hook 'visual-line-mode)

;; MoveText
(use-package move-text)
(move-text-default-bindings)

(use-package command-log-mode)

(use-package swiper :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode		
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x r j" . counsel-register)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

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

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
 :commands (magit-status magit-get-current-branch)
 :custom
 (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :ensure t
  :hook (go-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.opt\\'"))


(setq lsp-enable-file-watchers nil)
(use-package json-mode)
;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(setq gofmt-command "goimports")

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map "\"" 'electric-pair)
            (define-key go-mode-map "\'" 'electric-pair)
            (define-key go-mode-map "(" 'electric-pair)
            (define-key go-mode-map "[" 'electric-pair)
            (define-key go-mode-map "{" 'electric-pair)))

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(yas-global-mode 1)

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


(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)


(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(global-set-key (kbd "C-j") 'pop-global-mark)

(defun unpop-to-mark-command ()
    "Unpop off mark ring. Does nothing if mark ring is empty."
    (interactive)
    (when mark-ring
      (let ((pos (marker-position (car (last mark-ring)))))
        (if (not (= (point) pos))
            (goto-char pos)
          (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
          (set-marker (mark-marker) pos)
          (setq mark-ring (nbutlast mark-ring))
          (goto-char (marker-position (car (last mark-ring))))))))
(global-set-key (kbd "C-k") 'unpop-to-mark-command)


(use-package company
  :ensure t
  :config (progn
            ;; don't add any dely before trying to complete thing being typed
            ;; the call/response to gopls is asynchronous so this should have little
            ;; to no affect on edit latency
            (setq company-idle-delay 0)
            ;; start completing after a single character instead of 3
            (setq company-minimum-prefix-length 1)
            ;; align fields in completions
            (setq company-tooltip-align-annotations t)
            )
  )

(add-hook 'after-init-hook 'global-company-mode)                           

(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-k") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-j") #'company-select-next-or-abort))

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  )



(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun db/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . db/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(defun efs/presentation-setup()
  (setq text-scale-mode-amount 2)
  (org-display-inline-images)
  (text-scale-mode 1))

(defun efs/presentation-end()
  (text-scale-mode 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
	 (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation start")
  (org-tree-slide-deactive-message "Presentation end")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // " ))


(setq backup-directory-alist `(("." . "~/.saves")))

(setq org-src-tab-acts-natively t)

;; Open Pull requests from Magit

;; Set default browser as default OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;; original

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

;; Use Shift + arrow keys to move across windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

;; File Management
(setq insert-directory-program "gls" dired-use-ls-dired t)
;; Dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind(("C-x C-j" . dired-jump))
  :custom((dired-listing-switches "-lagho --group-directories-first"))
  :config
  (bind-key "C-p" #'dired-up-directory dired-mode-map)
  (bind-key "C-n" #'dired-find-file dired-mode-map)
  (bind-key "M-n" #'dired-find-file-other-window dired-mode-map)
)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; Use different font faces for different org mode blocks
(custom-theme-set-faces
 'user
 '(fixed-pitch ((t ( :family "Monospace"))))
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

(defun set-buffer-variable-pitch ()
  (interactive)
  (variable-pitch-mode t)
  (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  )

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) 
    (self-insert-command 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))

;; Prescient (sort results in minibuffer)
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(setq prescient-sort-length-enable nil)

(setq ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer counsel-find-file))

(setq ivy-prescient-retain-classic-highlighting t)

(setq prescient-filter-method '(literal initialism regexp))

;; Org Roam
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

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "QA" "|" "DONE")))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(setq org-startup-with-inline-images t)


(setq jiralib-url "https://formation.atlassian.net")

(add-to-list 'load-path "/path/to/jq-mode-dir")
(autoload 'jq-mode "jq-mode.el"
    "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

;; registers
(setq dotfiles-directory "~/.dotfiles/")
(set-register ?i (cons 'file (concat dotfiles-directory "init.el")))
(set-register ?w (cons 'file (concat dotfiles-directory "scripts/watch.sh")))

(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)


(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(setq-default tab-width 4)
(setq-default typescript-indent-level 2)

(global-anzu-mode +1)
(anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(setq lsp-go-use-gofumpt t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; dotenv-mode
(add-to-list 'auto-mode-alist '("\\.env\\'" . dotenv-mode))

(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(use-package tree-sitter
  :config
  (use-package tree-sitter-langs
    :config
    (add-to-list 'tree-sitter-major-mode-language-alist '(dockerfile-mode . dockerfile))))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(setq-default js-indent-level 2)

(provide 'init)

