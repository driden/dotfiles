#disable updates
export DISABLE_AUTO_UPDATE=true
# General
export PATH="$HOME/neovim/bin:/usr/bin:/bin:${PATH}"
#/usr/local/sbin:
#/usr/local/bin:
export PATH="${PATH}:$HOME/.emacs.d/bin"
export PATH="${PATH}:$HOME/.cargo/bin"
#export PATH="${PATH}:/usr/local/bin/python3"
export PATH="/opt/homebrew/bin:${PATH}"
export PATH="/opt/homebrew/sbin:$PATH"

export ZSH="$HOME/.oh-my-zsh"
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export BROWSER=/usr/bin/brave
export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
export EDITOR='nvim'
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export TERMINAL='kitty'

# GO
export GOPATH=$HOME/go
export GOPROXY=direct
export PATH=$PATH:$GOPATH/bin

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
fi


#
# Scripts
#
# [ -f ~/scripts/git-scripts.zsh ] && source ~/scripts/git-scripts.zsh
# [ -f ~/scripts/nvm.zsh ] && source ~/scripts/nvm.zsh
# [ -f ~/scripts/rkt.sh ] && source ~/scripts/rkt.sh

SCRIPTS=$HOME/scripts
if [[ -d  "$SCRIPTS" ]]
then
  for file in $(ls $SCRIPTS/*.{zsh,sh})
  do
    source "$file"
  done
fi

#WORKSCRIPTS=$HOME/workscripts
#if [[ -d  "$WORKSCRIPTS" ]]
#then
#  for file in $(ls $WORKSCRIPTS/*.{zsh,sh})
#  do
#    source "$file"
#  done
#fi

# Scripts
#
 [ -f ~/workscripts/jump.zsh ] && source  ~/workscripts/jump.zsh
 [ -f ~/workscripts/aliases.zsh ] && source ~/workscripts/aliases.zsh
 # [ -f ~/scripts/rkt.sh ] && source ~/scripts/rkt.sh

#
# ZSH settings
#
ZSH_DISABLE_COMPFIX="true"
ZSH_THEME="af-magic"
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
COMPLETION_WAITING_DOTS="true"

plugins=(git git-prompt fzf)

source $HOME/.oh-my-zsh/oh-my-zsh.sh

# User configuration
#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#  exec tmux
#fi

alias zshconfig="nvim ~/.zshrc"
alias ohmyzsh="nvim ~/.oh-my-zsh"
alias srcz="source $ZSH/oh-my-zsh.sh"
alias ll="ls -lah"
alias vimc="$EDITOR $HOME/.config/nvim/init.vim"
alias vim="$EDITOR"
alias vi="$EDITOR"
alias v="$EDITOR"
alias emacsc="emacsclient --create-frame"

# Git aliases
alias gs="git status"
alias gc="git commit"
alias gcm="git commit -m"
alias gap="git add -p"
alias gp="git pull"
alias gP="git push"
alias gtree="git log --oneline --decorate --all --graph"
alias lg="lazygit"

# default with ohmyzsh
unalias gco
unalias gbd

# dotfiles versioning with bare repo
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias refreshenv='source $HOME/.zshrc && source $HOME/.zshenv'

function init_fzf() {
  [ -f ~/.fzf/completion.zsh ] && source ~/.fzf/completion.zsh
  [ -f ~/.fzf/key-bindings.zsh ] && source ~/.fzf/key-bindings.zsh
}

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"


zvm_before_init_commands=()
zvm_after_init_commands+=(init_fzf)
zvm_before_select_vi_mode_commands=()
zvm_after_select_vi_mode_commands=()
export PATH=/Users/driden/.nvm/versions/node/v17.9.0/bin:/opt/homebrew/sbin:/opt/homebrew/bin:/Users/driden/neovim/bin:/usr/bin:/bin:/Library/Frameworks/Python.framework/Versions/3.10/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/Applications/kitty.app/Contents/MacOS:/Users/driden/.emacs.d/bin:/Users/driden/.cargo/bin:/Users/driden/go/bin:/Users/driden/.cabal/bin
