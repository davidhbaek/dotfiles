# Bootstrap a new Mac

#### 1. Install Apple's Command Line Tools, (needed for Git and Homebrew.)

```zsh
xcode-select --install
```
<br>

#### 2. Clone repo into new hidden directory.

```zsh
# Use SSH (if set up)...
git clone git@github.com:davidformation/dotfiles.git ~/.dotfiles
# ...or use HTTPS and switch remotes later.
git clone https://github.com/davidformation/dotfiles.git ~/.dotfiles
```
<br>

#### 3. Create symlinks in the Home directory to the real files in the repo.

```zsh
# There are better and less manual ways to do this;
# investigate install scripts and bootstrapping tools.
ln -s ~/.dotfiles/.zshrc ~/.zshrc
ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
```
<br>

#### 4. Install Homebrew, then whatever's in the Brewfile.

```zsh
# These could also be in an install script.
# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Then pass in the Brewfile location...
brew bundle --file ~/.dotfiles/Brewfile
# ...or move to the directory first.
cd ~/.dotfiles && brew bundle
```
<br>

#### 5. Install ohmyzsh
```
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```