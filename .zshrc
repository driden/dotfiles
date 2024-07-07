export HISTSIZE=10000

# General
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
# export PATH="$HOME/neovim/bin:/usr/bin:/bin:${PATH}"
export PATH="$HOME/neovim/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"
export GOPATH="$HOME/go"

# Binaries in case we have the folder
 if [ -d $HOME/.bin ]; then
  export PATH="$HOME/.bin:$PATH"
 fi

if [[ $(uname -p) == "arm" ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
else
  eval "$(/usr/local/homebrew/bin/brew shellenv)"
fi

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
fi

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export BROWSER=$(which firefox)
export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
export EDITOR='nvim'
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export TERMINAL='kitty'


if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
fi

#AWS
export AWS_PAGER=

SCRIPTS=$HOME/scripts
if [[ -d  "$SCRIPTS" ]]
then
  for file in $(ls $SCRIPTS/*.{zsh,sh})
  do
    source "$file"
  done
fi

# Scripts
#
 [ -f ~/workscripts/jump.zsh ] && source  ~/workscripts/jump.zsh
 [ -f ~/workscripts/aliases.zsh ] && source ~/workscripts/aliases.zsh

alias zshconfig="nvim ~/.zshrc"
alias refreshenv='source $HOME/.zshrc && source $HOME/.zshenv'
alias dot="cd $HOME/code/dotfiles && $EDITOR ."
alias ll="eza --long --all"
alias vim="$EDITOR"
alias vi="$EDITOR"
alias v="$EDITOR"
alias emacsc="emacsclient --create-frame"
alias lg=lazygit
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Git aliases
alias gs="git status"
alias gc="git commit"
alias gcm="git commit -m"
alias gap="git add -p"
alias gp="git pull"
alias gP="git push"
alias gtree="git log --oneline --decorate --all --graph"
alias glog="git log --oneline --decorate --all -n 10"
alias lg="lazygit"

# Terraform
alias tp="terraform plan"
alias ta="terraform apply"
alias tap="terraform apply -auto-aprove"
alias twl="terraform workspace list"
alias twc="terraform workspace list | grep '*' | tr -d '*'| tr -d '[:space:]'"

alias pip='pip3'

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env


#Plugin manager
# Download Znap, if it's not there yet.
local PLUGINS_BASE=$HOME/zsh-plugins/plugins
local PLUGINS_FOLDER=$PLUGINS_BASE/plugins
local ZNAP_FOLDER=$PLUGINS_BASE/znap

[[ -r $ZNAP_FOLDER/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $ZNAP_FOLDER
source $ZNAP_FOLDER/znap.zsh  # Start Znap
zstyle ':znap:*' repos-dir $PLUGINS_FOLDER
znap source chitoku-k/fzf-zsh-completions
znap source romkatv/zsh-defer
znap source zsh-users/zsh-syntax-highlighting

export SDKMAN_DIR="$HOME/.sdkman"
if [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]
then
  zsh-defer source "$SDKMAN_DIR/bin/sdkman-init.sh"
else
  echo "NO SDK"
fi

eval "$(fzf --zsh)"
eval "$(/usr/libexec/path_helper)"
eval "$(zoxide init zsh)"
eval "$(fnm env --use-on-cd)"
eval "$(starship init zsh)"

if [[ $(uname) -eq "Linux" ]]; then
  . "$HOME/.asdf/asdf.sh"
else 
  . /usr/local/opt/asdf/libexec/asdf.sh
fi

[[ -d "$PATH:/opt/nvim-linux64" ]] && export PATH="$PATH:/opt/nvim-linux64/bin"
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh"
