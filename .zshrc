# General
export PATH="$HOME/neovim/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/bin:${PATH}"
export PATH="${PATH}:$HOME/.emacs.d/bin"

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
[ -f ~/scripts/gbd.zsh ] && source ~/scripts/gbd.zsh
[ -f ~/scripts/nvm.zsh ] && source ~/scripts/nvm.zsh
[ -f ~/scripts/rkt.sh ] && source ~/scripts/rkt.sh

for file in $(ls $HOME/workscripts/*.{zsh,sh})
do
  source "$file"
done


#
# ZSH settings
#
ZSH_DISABLE_COMPFIX="true"
ZSH_THEME="af-magic"
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git git-prompt fzf zsh-vi-mode)

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
alias gs="git status"
alias gc="git commit"
alias ga="git add"
alias gpull="git pull"
alias gpush="git push"
alias gb="git branch"
alias gcb="git branch | fzf | xargs git checkout"
alias gtree="git log --oneline --decorate --all --graph"

# dotfiles versioning with bare repo
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias refreshenv='source $HOME/.zshenv'
alias rc='source $HOME/.zshrc'


export PATH=$HOME/.toolbox/bin:$PATH

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/lrrezend/.sdkman"
[[ -s "/Users/lrrezend/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/lrrezend/.sdkman/bin/sdkman-init.sh"

function init_fzf() {
  [ -f ~/.fzf/completion.zsh ] && source ~/.fzf/completion.zsh
  [ -f ~/.fzf/key-bindings.zsh ] && source ~/.fzf/key-bindings.zsh
}

zvm_before_init_commands=()
zvm_after_init_commands+=(init_fzf)
zvm_before_select_vi_mode_commands=()
zvm_after_select_vi_mode_commands=()
