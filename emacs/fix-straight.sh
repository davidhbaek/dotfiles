#!/bin/bash
# Fix straight.el connection issues

# Create the directory structure for the problematic repository
mkdir -p ~/.emacs.d/straight/repos/nongnu-elpa/.git

# Create a basic .git config to pretend it's a git repository
cat > ~/.emacs.d/straight/repos/nongnu-elpa/.git/config << EOF
[core]
        repositoryformatversion = 0
        filemode = true
        bare = false
        logallrefupdates = true
        ignorecase = true
        precomposeunicode = true
EOF

# Create a .no-pull file to tell straight.el not to try updating it
touch ~/.emacs.d/straight/repos/nongnu-elpa/.no-pull

# Create an empty recipe file
mkdir -p ~/.emacs.d/straight/repos/nongnu-elpa/recipes
echo "()" > ~/.emacs.d/straight/repos/nongnu-elpa/recipes/.recipe

echo "Straight.el fix applied! Now copy the fixed init.el to your .emacs.d directory:"
echo "cp ~/.dotfiles/emacs/init.el ~/.emacs.d/init.el"
