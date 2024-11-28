;;; db-file-management.el --- File management configurations

;;; Commentary:
;; Configurations for file management via the Dired package

;;; Code:


;; ====================================
;; File Management (Dired)
;; ====================================

;; Basic dired configuration
(setq insert-directory-program "gls" dired-use-ls-dired t)

;; Main dired configuration
(use-package dired
  :ensure nil  ; Built into Emacs
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))  ; Quick jump to current file's directory
  :custom ((dired-listing-switches "-lagho --group-directories-first"))  ; Better listing format
  :config
  ;; Custom navigation keys in dired
  (bind-key "C-p" #'dired-up-directory dired-mode-map)        ; Go up directory with C-p
  (bind-key "C-n" #'dired-find-file dired-mode-map)          ; Open file/dir with C-n
  (bind-key "M-n" #'dired-find-file-other-window dired-mode-map))  ; Open in new window

;; Enhanced directory tree functionality
(use-package dired-subtree
  :after dired
  :config
  ;; Toggle subtree view with TAB
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; macOS specific dired configuration
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))  ; Fix for macOS ls command

(provide 'db-file-management)
;;; db-file-management.el ends here
