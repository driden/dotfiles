# General
export PATH="$HOME/neovim/bin:/usr/bin:/bin:${PATH}"
export PATH="${PATH}:$HOME/.emacs.d/bin"
export PATH="${PATH}:$HOME/.cargo/bin"
export PATH="${PATH}:$HOME/.ghcup/bin/"
export PATH="${PATH}:$GOPATH"
export PATH="${PATH}:.local/bin"

# Binaries in case we have the folder
 if [ -d $HOME/.bin ]; then
  export PATH="${PATH}:$HOME/.bin"
 fi

OS=$(uname)
BREW_PREFIX=
if [[ $(uname -p) == "arm" ]]; then
  BREW_PREFIX=/opt/homebrew
# Important for mac M1
alias ibrew='arch -x86_64 /usr/local/bin/brew'
else
  BREW_PREFIX=/usr/local
  HOMEBREW_CELLAR=/usr/local/Cellar
fi

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
fi

export PATH=$BREW_PREFIX/sbin:$BREW_PREFIX/bin:$PATH

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export BROWSER=$(which firefox)
export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
export EDITOR='nvim'
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export TERMINAL='WezTerm'

# GO
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

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
znap source romkatv/zsh-defer
znap source chitoku-k/fzf-zsh-completions
znap source zsh-users/zsh-syntax-highlighting

export SDKMAN_DIR="$HOME/.sdkman"
if [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]
then
  zsh-defer source "$SDKMAN_DIR/bin/sdkman-init.sh"
fi

eval "$(fzf --zsh)"
# function init_fzf() {
#   [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#   [ -f ~/.fzf/completion.zsh ] && source ~/.fzf/completion.zsh
#   [ -f ~/.fzf/key-bindings.zsh ] && source ~/.fzf/key-bindings.zsh
# }
# [[ -d "$HOME/.fzf" ]] && init_fzf

zsh-defer eval "$(zoxide init zsh)"
zsh-defer eval "$(fnm env --use-on-cd --fnm-dir $HOME/.local/share/fnm)"
eval "$(starship init zsh)"

case "$OS" in
  Linux)
    . "$HOME/.asdf/asdf.sh"
    ;;
  Darwin)
    . /usr/local/opt/asdf/libexec/asdf.sh
    ;;
esac
