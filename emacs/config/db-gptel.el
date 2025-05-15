;; This file is part of the Emacs configuration for using GPTel with tools
(straight-use-package 'gptel)

(use-package gptel
  :config

(setq gptel-model 'claude-3.7-sonnet
      gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Optional: Enable Org mode as the default mode for gptel buffers
  (setq gptel-default-mode 'org-mode)

  ;; Optional: Keybindings for quick access
  :bind (("C-c g s" . gptel-send)   ;; Send query
         ("C-c g c" . gptel)))     ;; Open dedicated chat buffer

(require 'gptel-integrations)

(provide 'db-gptel)
