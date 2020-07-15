# General
export PATH="$HOME/neovim/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin:${PATH}"
 
export ZSH="$HOME/.oh-my-zsh"
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export BROWSER=/usr/bin/firefox
export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
export EDITOR='nvim'
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
fi

#
# Work
#
[ -f ~/.work.zsh ] && source ~/.work.zsh

#
# ZSH settings
#
ZSH_DISABLE_COMPFIX="true"
ZSH_THEME="sorin"
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git git-prompt battery npm node fzf)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $HOME/.oh-my-zsh/oh-my-zsh.sh

# User configuration
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

alias zshconfig="nvim ~/.zshrc"
alias ohmyzsh="nvim ~/.oh-my-zsh"
alias srcz="source $ZSH/oh-my-zsh.sh"
alias ll="ls -lah"
alias vimc="$EDITOR $HOME/.config/nvim/init.vim"
alias vim="$EDITOR"
alias vi="$EDITOR"
alias v="$EDITOR"

# dotfiles versioning with bare repo
alias dotfiles='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

