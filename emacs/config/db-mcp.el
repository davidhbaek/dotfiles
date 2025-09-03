;; Add mcp.el to load path (manually installed from GitHub)
(add-to-list 'load-path "~/.emacs.d/packages/mcp.el")

;; Now we can require and use it
(require 'mcp)
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
