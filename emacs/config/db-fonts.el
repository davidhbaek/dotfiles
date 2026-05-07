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

;; Set font at startup
(db/set-default-font)

;; Force the default font configuration to take precedence
;; This is run at a hook to ensure it happens after other configuration
(defun db/enforce-font-settings ()
  "Ensure font settings are applied correctly at startup."
  (db/set-default-font))

;; Add to startup hook to ensure fonts are set correctly
(add-hook 'after-init-hook #'db/enforce-font-settings)

(provide 'db-fonts)
;;; db-fonts.el ends here
