;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package with straight.el
(straight-use-package 'use-package)
(require 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Dependencies for copilot-chat
(use-package markdown-mode)
(use-package shell-maker)
(use-package aio)

;; Copilot configuration
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  ;; Increase max characters
  (setq copilot-max-char 200000)
  ;; Key bindings
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; Copilot Chat configuration
(use-package copilot-chat
  :after copilot
  :ensure t
  :demand t  ;; Force immediate loading
  :init
  ;; Make sure the package is loaded before using any variables
  (require 'copilot-chat)
  :config
  ;; Set model if needed
  (setq copilot-chat-model "gpt-4")
  ;; Use org frontend 
  (setq copilot-chat-frontend 'org)
  
  ;; Key bindings
  :bind (("C-c C-p" . copilot-chat)
         ("C-c C-f" . copilot-chat-focus)))


;; Install acme-theme if not already installed
(unless (package-installed-p 'acme-theme)
  (package-refresh-contents)
  (package-install 'acme-theme))

;; Load the Acme theme for CoPilot
(require 'acme-theme)

;; Your existing Zenburn theme likely remains the default theme
;; But CoPilot code blocks will use Acme theme colors

;; Define custom faces for CoPilot code blocks using Acme theme colors
(defface copilot-code-block-face
  '((t (:background "#f6f8fa" :foreground "#181818"))) ; Acme light background and text color
  "Face for CoPilot code blocks using Acme theme colors.")

;; Apply the custom face to org-mode source blocks in CoPilot
(defun my/copilot-org-src-block-setup ()
  (add-to-list 'org-src-block-faces '(".*" . copilot-code-block-face)))

(add-hook 'org-mode-hook 'my/copilot-org-src-block-setup)

(provide 'db-copilot)
