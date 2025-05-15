;;; db-fonts.el --- Font stuff

;;; Commentary:
;; Configuration for fonts in Emacs

;;; Code:

;; Set JetBrains Mono as the default font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 150
                    :weight 'normal)

;; Ensure Org mode uses JetBrains Mono for fixed-pitch
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono")

;; Force the default font configuration to take precedence
;; This is run at a hook to ensure it happens after other configuration
(defun db/enforce-font-settings ()
  "Ensure font settings are applied correctly at startup."
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 150
                      :weight 'normal))

;; Add to startup hook to ensure fonts are set correctly
(add-hook 'after-init-hook #'db/enforce-font-settings)

(provide 'db-fonts)
;;; db-fonts.el ends here
