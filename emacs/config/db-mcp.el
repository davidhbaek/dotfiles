;; If you use straight.el
(straight-use-package '(mcp :type git :host github :repo "lizqwerscott/mcp.el"))
(use-package mcp)
(require 'mcp-hub)

;; Configure servers with both filesystem and websearch
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx" 
                         :args ("@modelcontextprotocol/server-filesystem" 
                                "/Users/david.baek/Desktop" 
                                "/Users/david.baek/.dotfiles")))))

;; Start all servers after Emacs initializes
(add-hook 'after-init-hook #'mcp-hub-start-all-server)

(provide 'db-mcp)
