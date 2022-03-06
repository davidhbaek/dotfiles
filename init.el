(setq inhibit-startup-message t)
(toggle-scroll-bar -1) ; Disable scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room

(menu-bar-mode -1)     ; Disable the menu bar

(set-face-attribute 'default nil :height 180)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
 ("org" . "https://orgmode.org/elpa/")
 ("elpa" . "https://elpa.gnu.org/packages/")))

;; Pick a theme to use here
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
(global-linum-mode t)

;; Disable line numbers for org mode
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))

;; Allow for multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

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

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

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
  :config
  (lsp-enable-which-key-integration t))

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-tree-slide magit-gh-pulls terraform-mode graphql-mode org-superstar yaml-mode move-text org-bullets multiple-cursors company yasnippet go-mode zenburn-theme which-key use-package rainbow-delimiters magit lsp-ui lsp-ivy helpful doom-themes doom-modeline counsel-projectile command-log-mode all-the-icons-ivy-rich)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-whole-line)


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

(use-package org-tree-slide
  :custom
  (org-imagine-actual-width nil))

;;(setenv "PATH" (concat (getenv "PATH") ":/Users/david.baek/go"))

(setenv "GOPATH" "/Users/david.baek/go")
(setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/david.baek/go/bin:/Users/david.baek/go/bin")

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq backup-directory-alist `(("." . "~/.saves")))


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
