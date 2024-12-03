;;; db-terminal.el --- Terminal configuration for Emacs

;;; Commentary:
;; Terminal configuration with vterm and Powerlevel10k support

;;; Code:

(use-package vterm
  :ensure t
  :when (or (executable-find "cmake") (executable-find "gmake") (executable-find "make"))
  :bind
  ("C-c t" . vterm-other-window)
  :custom
  (vterm-shell "/bin/zsh")
  (shell-file-name "/bin/zsh")
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-timer-delay 0.01)
  (vterm-term-environment-variable "xterm-256color")
  (vterm-clear-scrollback-when-clearing t)
  :config
  ;; Basic terminal key bindings
  (define-key vterm-mode-map (kbd "C-c C-t") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-c C-y") 'vterm-yank)

  ;; Handle backspace and deletion in copy mode
  (define-key vterm-copy-mode-map [backspace] 'vterm-send-backspace)
  (define-key vterm-copy-mode-map [delete] 'vterm-send-delete)
  (define-key vterm-copy-mode-map (kbd "DEL") 'vterm-send-backspace)
  
  ;; Enhanced copy/paste functionality
  (define-key vterm-copy-mode-map (kbd "M-w")
              (lambda ()
                (interactive)
                (kill-ring-save (mark) (point))
                (vterm-copy-mode -1)))
  
  (define-key vterm-copy-mode-map (kbd "C-w")
              (lambda ()
                (interactive)
                (with-current-buffer (current-buffer)
                  (kill-ring-save (mark) (point))
                  (vterm-copy-mode -1)
                  (vterm-send-C-k))))  ; Simulates delete to end of line
  
  ;; Terminal setup hook
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable line numbers
              (display-line-numbers-mode -1)
              ;; Set terminal environment
              (setenv "TERM" "xterm-256color")
              (setenv "INSIDE_EMACS" "vterm")
              ;; Terminal buffer settings
              (setq-local scroll-margin 0)
              (setq-local scroll-conservatively 101)
              (setq-local global-hl-line-mode nil)))

  ;; Enable mouse support in terminal mode
  (unless window-system
    (xterm-mouse-mode 1)))

(provide 'db-terminal)
;;; db-terminal.el ends here
