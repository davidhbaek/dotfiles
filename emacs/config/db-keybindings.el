;;; db-keybindings.el --- Key bindings configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Key Bindings
;; ====================================

;; Move text up/down
(use-package move-text
  :defer
  :ensure t)
(move-text-default-bindings)  ; M-up/M-down to move lines/regions

;; Multiple cursors for simultaneous editing
(use-package multiple-cursors
  :defer
  :bind
  (("C->" . mc/mark-next-like-this)         ; Mark next occurrence
   ("C-<" . mc/mark-previous-like-this)     ; Mark previous occurrence
   ("C-?" . mc/mark-all-like-this)          ; Mark all occurrences
   ("C-S-c C-S-c" . mc/edit-lines)))        ; Add cursor to each line in region

;; Custom global key bindings
(global-set-key (kbd "C-w") 'kill-ring-save)  ; Copy (different from default)
(global-set-key (kbd "M-w") 'kill-region)     ; Cut (different from default)
(global-set-key (kbd "C-j") 'pop-global-mark) ; Jump to previous mark
(global-set-key (kbd "C-k") 'unpop-to-mark-command)  ; Jump forward in mark ring

;; Window movement
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))  ; Use Shift+arrows to move between windows

(provide 'db-keybindings)
;;; db-keybindings.el ends here
