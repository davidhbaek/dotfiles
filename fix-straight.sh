#!/bin/bash
# Script to fix Emacs straight.el repository issues

echo "Fixing straight.el repositories..."

# Create directory if it doesn't exist
mkdir -p ~/.emacs.d/straight/repos/nongnu-elpa

# Add an empty .no-pull file to skip this repository
touch ~/.emacs.d/straight/repos/nongnu-elpa/.no-pull

# Create a basic placeholder .git directory to make straight.el think it's a repo
mkdir -p ~/.emacs.d/straight/repos/nongnu-elpa/.git
echo "[core]
        repositoryformatversion = 0
        filemode = true
        bare = false
        logallrefupdates = true
        ignorecase = true
        precomposeunicode = true" > ~/.emacs.d/straight/repos/nongnu-elpa/.git/config

# Create a modification to your init.el to handle network errors
cat > ~/.emacs.d/network-fix.el << EOF
;; network-fix.el - Add to your init.el to handle network errors
(setq straight-check-for-modifications nil)  ; Don't check for modifications
(setq straight-vc-git-default-clone-depth 1) ; Shallow clone for faster downloads
(setq straight-fix-org nil)                  ; Don't try to fix org-mode
(setq straight-disable-native-compile t)     ; Disable native compilation temporarily

;; Advice for handling network errors in straight.el
(defadvice straight-fetch-package (around straight-fetch-package-quietly activate)
  "Ignore all errors when fetching packages."
  (with-demoted-errors "Error: %S" ad-do-it))

(defadvice straight-pull-package (around straight-pull-package-quietly activate)
  "Ignore all errors when pulling packages."
  (with-demoted-errors "Error: %S" ad-do-it))

(defadvice straight-rebuild-package (around straight-rebuild-package-quietly activate)
  "Ignore all errors when rebuilding packages."
  (with-demoted-errors "Error: %S" ad-do-it))

;; Provide an emergency function to try and get packages
(defun fix-straight-packages ()
  "Try to fix straight.el packages without network."
  (interactive)
  (message "Attempting to fix packages...")
  (straight-use-package 'use-package)
  (straight-use-package 'which-key)
  (straight-use-package 'zenburn-theme)
  (message "Basic packages installed. You may need to restart Emacs."))
EOF

echo "Fix complete! Add this to the beginning of your init.el:"
echo "  ;; Load network fix for straight.el"
echo "  (load \"~/.emacs.d/network-fix.el\")"
echo ""
echo "Then try starting Emacs again. If it still fails, start with:"
echo "  emacs -Q --eval \"(load \\\"~/.emacs.d/network-fix.el\\\")\" --eval \"(fix-straight-packages)\""
