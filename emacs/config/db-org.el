;;; db-org.el --- Org mode configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Org Mode Configuration
;; ====================================

;; Basic org-mode setup function
(defun db/org-mode-setup()
  "Configure basic org-mode settings and appearance."
  (org-indent-mode)              ; Enable indentation
  (variable-pitch-mode 1)        ; Use variable-pitch fonts
  (auto-fill-mode 0)             ; Disable auto-fill
  (visual-line-mode 1))          ; Enable visual line mode

;; Main org-mode configuration
(use-package org
  :hook (org-mode . db/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")           ; Use this symbol for collapsed sections
  (setq org-hide-emphasis-markers t)  ; Hide markup symbols
  (setq org-src-tab-acts-natively t)) ; Better tab behavior in source blocks

;; Custom bullet appearance for lists
(font-lock-add-keywords 'org-mode
                       '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Configure org todo states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "QA" "|" "DONE")))

;; Enable inline images by default
(setq org-startup-with-inline-images t)

;; Org face configuration for better typography
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (headline           `(:inherit default)))
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; Better bullet points in org-mode
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org-roam configuration for note-taking
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org-roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Presentation mode configuration
(defun efs/presentation-setup()
  "Setup environment for org-mode presentations."
  (setq text-scale-mode-amount 2)
  (org-display-inline-images)
  (text-scale-mode 1))

(defun efs/presentation-end()
  "Cleanup after org-mode presentations."
  (text-scale-mode 0))

;; Presentation tool configuration
(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation start")
  (org-tree-slide-deactive-message "Presentation end")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // "))

(provide 'db-org)
;;; db-org.el ends here