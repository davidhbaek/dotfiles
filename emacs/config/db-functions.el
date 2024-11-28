;;; db-functions.el --- Custom function configurations

;;; Commentary:
;; Configurations for custom functions

;;; Code:


;; ====================================
;; Custom Functions
;; ====================================

;; Navigation in mark ring
(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Useful for navigating back to previous positions."
  (interactive)
  (set-mark-command t))

;; Enhanced mark ring navigation
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty.
Allows for navigating forward in mark history."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))

;; Smart paired character insertion
(defun electric-pair ()
  "Insert character pair without surrounding spaces at end of line.
Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair))
    (self-insert-command 1)))

(provide 'db-functions)
;;; db-functions.el ends here
