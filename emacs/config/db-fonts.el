;;; db-fonts.el --- Font stuff

;;; Commentary:
;; Configuration for fonts in Emacs

;;; Code:

;; Function to check if a font is available
(defun db/font-exists-p (font-name)
  "Check if FONT-NAME is available on the system."
  (find-font (font-spec :name font-name)))

;; Set the default font with fallback support
(defun db/set-default-font ()
  "Set default font to JetBrains Mono with fallback to Menlo."
  (cond
   ((db/font-exists-p "JetBrains Mono")
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :height 150
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono")
    (message "Font set to JetBrains Mono"))
   ((db/font-exists-p "Menlo")
    (set-face-attribute 'default nil
                        :family "Menlo"
                        :height 150
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family "Menlo")
    (message "JetBrains Mono not found. Using Menlo as fallback."))
   (t
    (message "Warning: Neither JetBrains Mono nor Menlo found. Using system default font."))))

;; Defer font setup until a graphical frame exists (required for daemon mode)
(defun db/setup-fonts-for-frame (&optional frame)
  "Apply font settings to FRAME, or current frame if nil."
  (when (display-graphic-p (or frame (selected-frame)))
    (with-selected-frame (or frame (selected-frame))
      (db/set-default-font))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'db/setup-fonts-for-frame)
  (add-hook 'after-init-hook #'db/setup-fonts-for-frame))

(provide 'db-fonts)
;;; db-fonts.el ends here
