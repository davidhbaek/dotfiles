;; If you use straight.el
;; (straight-use-package '(mcp :type git :host github :repo "lizqwerscott/mcp.el"))
;; Note: MCP package needs to be installed manually or via package.el
;; Commenting out for now since we use package.el, not straight.el
;; (use-package mcp)
;; (require 'mcp-hub)

;; MCP configuration commented out until package is properly installed
;; Configure servers with both filesystem and websearch
;; (setq mcp-hub-servers
;;       '(("filesystem" . (:command "npx" 
;;                          :args ("@modelcontextprotocol/server-filesystem" 
;;                                 "/Users/davidbaek/Desktop" 
;;                                 "/Users/davidbaek/.dotfiles")))))

;; Start all servers after Emacs initializes
;; (add-hook 'after-init-hook #'mcp-hub-start-all-server)

(provide 'db-mcp)
