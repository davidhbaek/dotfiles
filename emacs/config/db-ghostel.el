;;; db-ghostel.el --- Terminal configuration with ghostel -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal emulator powered by ghostel (libghostty-vt).  Replaces the
;; previous vterm setup (see db-terminal.el, no longer required in init.el).
;;
;; Notes vs the old vterm config:
;; - No build toolchain: ghostel auto-downloads its prebuilt native module.
;; - TERM is managed by ghostel itself (xterm-ghostty); don't override it.
;; - zsh shell integration is built-in and on by default (directory tracking,
;;   prompt navigation) — no shell rc changes needed.
;; - Buffers are auto-named from the terminal title, so no name-string var.
;; - Copy mode (C-c C-t) is a normal read-only buffer, so plain M-w works.

;;; Code:

;; Defined at top level (not inside the use-package :config below) so the
;; `:bind' entry resolves to a real command before ghostel is loaded.
(defun db/ghostel-new ()
  "Open a new ghostel terminal in a fresh buffer.
Passes a non-numeric prefix arg to `ghostel', which always creates a
new terminal rather than switching to an existing one."
  (interactive)
  (ghostel '(4)))

(use-package ghostel
  :ensure t
  :bind
  ("C-c t" . db/ghostel-new)
  :custom
  (ghostel-shell "/bin/zsh")
  (shell-file-name "/bin/zsh")
  (ghostel-max-scrollback (* 10 1024 1024)) ; ~10k rows on an 80-col terminal
  :config
  ;; Terminal setup hook
  (add-hook 'ghostel-mode-hook
            (lambda ()
              ;; Disable line numbers
              (display-line-numbers-mode -1)
              ;; Terminal buffer settings
              (setq-local scroll-margin 0)
              (setq-local scroll-conservatively 101)
              (setq-local global-hl-line-mode nil))))

(provide 'db-ghostel)
;;; db-ghostel.el ends here
