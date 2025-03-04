# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
export PATH=$PATH:$HOME/go/bin
export GOPATH=$HOME/go
export PATH="${PATH}:${GOPATH}/bin"

# Path to your oh-my-zsh installation.
export ZSH="/Users/david.baek/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(zsh-autosuggestions direnv zsh-syntax-highlighting)
# User configuration
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"
 # Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias platform-dev="aws-okta exec formation-platform-dev -- /bin/zsh"
alias platform-prod="aws-okta exec formation-platform-prod -- /bin/zsh"
alias ghci='stack ghci'
alias glc="git rev-parse HEAD | pbcopy | git rev-parse HEAD"
alias emacs-gnu='$(/Applications/Emacs.app/Contents/MacOS/Emacs "$@")'
# alias act because of the M1 chip warning
alias act="act --container-architecture linux/amd64"

# No more writing 3
alias pip="pip3"
alias python="python3"

# or awslocal
alias awsl="awslocal"

# run the watchexec commands for Go development
alias gob="$HOME/bin/gob.sh"


# alias for Tackle to set AWS_PROFILE, login to code artifact to pull libraries, and log in to SSO
alias start-day='(export AWS_PROFILE=mgmt-appeng && aws sso login && aws codeartifact login --tool pip --domain tackle-codeartifact --repository tackle-codeartifact-pypi --domain-owner 730998372749 --region us-west-2 && aws ecr --profile mgmt-appeng get-login-password | docker login --username AWS --password-stdin 730998372749.dkr.ecr.us-west-2.amazonaws.com)'
FUNCTIONS_CORE_TOOLS_TELEMETRY_OPTOUT='true' # opt out of sending Azure Functions Core CLI usage

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
fpath=( ~/.zfunc $fpath )
autoload -Uz ~/.zfunc/wa
autoload -Uz ~/.zfunc/dkr
export TERM=xterm-256color
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
source $ZSH/oh-my-zsh.sh
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
#[ -f "/Users/david.baek/.ghcup/env" ] && source "/Users/david.baek/.ghcup/env" # ghcup-env

# source /usr/local/share/chruby/chruby.sh

[ -f "/Users/david.baek/.ghcup/env" ] && source "/Users/david.baek/.ghcup/env" # ghcup-env


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
