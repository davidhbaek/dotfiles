;; This file is part of the Emacs configuration for using GPTel with tools
;; Using standard package.el

(use-package gptel
  :config

(setq gptel-model 'qwen2.5:32b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen2.5:32b)))

  ;; Optional: Enable Org mode as the default mode for gptel buffers
  (setq gptel-default-mode 'org-mode)

  ;; Optional: Keybindings for quick access
  :bind (("C-c g s" . gptel-send)   ;; Send query
         ("C-c g c" . gptel)))     ;; Open dedicated chat buffer

;; gptel-integrations doesn't exist in standard gptel package
;; (require 'gptel-integrations)

(provide 'db-gptel)
