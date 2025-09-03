;; Copilot is now available on MELPA - no need for straight.el

;; Dependencies for copilot-chat
(use-package markdown-mode)
(use-package shell-maker)
(use-package aio)

;; Copilot configuration
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  ;; Increase max characters to handle larger files
  (setq copilot-max-char 500000)
  ;; Add indentation configuration to prevent warnings
  (setq copilot-indent-offset-warning-disable t)
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
