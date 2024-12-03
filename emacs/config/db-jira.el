;;; db-jira.el --- Jira configuration

;;; Commentary:
;;

;;; Code:

(setq jiralib-url "https://tackle.atlassian.net")

(use-package org-jira
  :ensure t
  :defer t
  :custom
  ;; Configure todo keywords to match your workflow
  (org-todo-keywords
   '((sequence "TODO" "IN PROGRESS" "IN REVIEW" "DONE")))
  
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "firebrick2" :weight bold))
     ("IN PROGRESS" . (:foreground "orange" :weight bold))
     ("IN REVIEW" . (:foreground "dodger blue" :weight bold))
     ("DONE" . (:foreground "forest green" :weight bold))))
  
  ;; Title and property formatting
  (org-jira-title-format "** %s %s")
  (org-jira-property-format ":%s: %s")
  (org-jira-property-list
   '("assignee" "reporter" "type" "priority" "status" "created" "updated"))
  (org-jira-date-format "%Y-%m-%d %H:%M")
  (org-hidden-properties '("ID" "CUSTOM_ID" "FILENAME"))
  
  :config
  ;; Custom function for fetching team tickets
  (defun my-get-team-tickets ()
    "Get tickets for DINO team."
    (interactive)
    (org-jira-get-issues-by-custom-jql
     "project = DINO AND assignee = currentUser() ORDER BY updated DESC"))
  
  ;; Buffer setup function
  (defun my-org-jira-buffer-setup ()
    "Setup org-jira buffer for better readability"
    (org-indent-mode 1)
    (variable-pitch-mode 1)
    (setq-local org-startup-folded 'content)
    (setq-local org-startup-indented t)
    (setq-local org-indent-indentation-per-level 2))
  
  ;; Add buffer setup to org-jira-mode-hook
  (add-hook 'org-jira-mode-hook #'my-org-jira-buffer-setup)
  
  :bind
  (("C-c j t" . my-get-team-tickets)
   ("C-c j i" . org-jira-get-issues)
   ("C-c j c" . org-jira-create-issue)
   ("C-c j u" . org-jira-update-issue)
   ("C-c j a" . org-jira-assign-issue)
   ("C-c j m" . org-jira-comment-issue)))

(provide 'db-jira)
;;; db-jira.el ends here
