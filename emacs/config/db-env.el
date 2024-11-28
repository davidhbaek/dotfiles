;;; db-env.el --- Environment configurations

;;; Commentary:
;; Setting up PATHs and file registers

;;; Code:

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

(provide 'db-env)
;;; db-env.el ends here
