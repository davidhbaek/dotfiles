# Backup the brews installed on your machine

#### 1. Create Brewfile

```zsh
brew bundle dump
```

# Bootstrap a new Mac

#### 1. Install Apple's Command Line Tools, (needed for Git and Homebrew.)

```zsh
xcode-select --install
```
<br>

#### 2. Clone repo into new hidden directory.

```zsh
git clone git@github.com:davidhbaek/dotfiles.git ~/.dotfiles
```
<br>

#### 3. Create symlinks in the Home directory to the real files in the repo.

```zsh
ln -s ~/.dotfiles/.zshrc ~/.zshrc
ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
ln -s ~/.dotfiles/.zprofile ~/.zprofile
```
<br>

#### 4. Install Homebrew, then whatever's in the Brewfile.

```zsh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

```zsh
brew bundle --file ~/.dotfiles/Brewfile
```
<br>

#### 5. Install ohmyzsh
```
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```
