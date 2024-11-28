;;; db-navigation.el --- Navigation and Completion configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Navigation and Completion
;; ====================================

;; Ivy - Completion framework
(use-package ivy
  :diminish  ; Don't show mode in modeline
  :bind (("C-s" . swiper)  ; Better search
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))  ; Enable ivy globally

;; Counsel - Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)         ; Enhanced M-x
         ("C-x b" . counsel-ibuffer)    ; Enhanced buffer switching
         ("C-x C-f" . counsel-find-file)  ; Enhanced file opening
         ("C-x r j" . counsel-register)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ; Don't start searches with ^

;; Enhanced minibuffer documentation
(use-package ivy-rich
  :defer
  :init (ivy-rich-mode 1))

;; Icons in ivy
(use-package all-the-icons-ivy-rich
  :defer
  :init (all-the-icons-ivy-rich-mode 1))

;; Better help system
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Better sorting for ivy
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (setq prescient-sort-length-enable nil
        ivy-prescient-retain-classic-highlighting t
        prescient-filter-method '(literal initialism regexp)))

;; Better search and replace feedback
(global-anzu-mode +1)
(anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(provide 'db-navigation)
;;; db-navigation.el ends here
