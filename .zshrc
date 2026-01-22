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


function add_dir_to_path() {
 if [ -d "$1" ]; then
  export PATH="$1:$PATH"
 fi
}
add_dir_to_path "$HOME/.fzf/bin"

# Binaries in case we have the folder
 if [ -d $HOME/.bin ]; then
  export PATH="$HOME/.bin:$PATH"
 fi

if [[ $(uname -p) == "arm" ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
# else
  # eval "$(/usr/local/homebrew/bin/brew shellenv)"
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
export FZF_ALT_C_COMMAND='fd --absolute-path --type d --max-depth 2 -E Library -E Pictures -E Music -E Applications -E zsh-plugins -E go . "$HOME"'

# FZF Theme Configuration - Define color palettes
typeset -A fzf_bamboo_colors=(
  bg "#252623"
  bg_plus "#383b35"
  preview_bg "#1c1e1b"
  fg "#f1e9d2"
  fg_plus "#f1e9d2"
  hl "#e75a7c"
  hl_plus "#e75a7c"
  info "#aaaaff"
  prompt "#8fb573"
  pointer "#dbb651"
  marker "#70c2be"
  spinner "#57a5e5"
  header "#8fb573"
  border "#8fb573"
)

typeset -A fzf_catppuccin_colors=(
  bg "#1e1e2e"
  bg_plus "#313244"
  preview_bg "#181825"
  fg "#cdd6f4"
  fg_plus "#cdd6f4"
  hl "#f38ba8"
  hl_plus "#f38ba8"
  info "#cba6f7"
  prompt "#cba6f7"
  pointer "#f5e0dc"
  marker "#f5e0dc"
  spinner "#f5e0dc"
  header "#94e2d5"
  border "#b4befe"
)

# Function to build FZF color options from theme
function _fzf_build_colors() {
  local theme=$1
  local array_name="fzf_${theme}_colors"
  
  # Use parameter expansion to access the associative array
  local bg="${${(P)array_name}[bg]}"
  local bg_plus="${${(P)array_name}[bg_plus]}"
  local preview_bg="${${(P)array_name}[preview_bg]}"
  local fg="${${(P)array_name}[fg]}"
  local fg_plus="${${(P)array_name}[fg_plus]}"
  local hl="${${(P)array_name}[hl]}"
  local hl_plus="${${(P)array_name}[hl_plus]}"
  local info="${${(P)array_name}[info]}"
  local prompt="${${(P)array_name}[prompt]}"
  local pointer="${${(P)array_name}[pointer]}"
  local marker="${${(P)array_name}[marker]}"
  local spinner="${${(P)array_name}[spinner]}"
  local header="${${(P)array_name}[header]}"
  local border="${${(P)array_name}[border]}"
  
  echo "--color=bg+:${bg_plus},bg:${bg},preview-bg:${preview_bg} \
--color=fg+:${fg_plus},fg:${fg},hl+:${hl_plus},hl:${hl} \
--color=info:${info},prompt:${prompt},pointer:${pointer},marker:${marker} \
--color=spinner:${spinner},header:${header},border:${border}"
}

# Build FZF options with theme colors
FZF_THEME_COLORS="$(_fzf_build_colors ${THEME:-bamboo})"

# Apply theme to all FZF commands
export FZF_DEFAULT_OPTS="--layout=reverse --border=rounded ${FZF_THEME_COLORS}"

# Alt-C: Directory navigation
export FZF_ALT_C_OPTS="--preview 'eza --icons --color=always --tree --level=1 {}' --preview-window=right:50%:wrap"

export EDITOR='nvim'
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export TERMINAL='wezterm'

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
fi

#AWS
export AWS_PAGER=

# Theme configuration - change this to switch themes
export THEME="bamboo"  # Options: "bamboo", "catppuccin"

for SCRIPTS in $HOME/scripts $HOME/workscripts; do
    if [[ -d  "$SCRIPTS" ]]; then
        for file in $SCRIPTS/*.{zsh,sh}(.N) # (.N) sets the null glob option and will expand only to files that match
      do
        [[ -f "$file" ]] && source "$file"
      done
    fi
done



alias zshconfig="nvim ~/.zshrc"
alias refreshenv='source $HOME/.zshrc && source $HOME/.zshenv'
alias dot="cd $HOME/code/dotfiles"
alias ll="eza --long --all --icons always"
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

# if [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]
# then
#   export SDKMAN_DIR="$HOME/.sdkman"
#   zsh-defer source "$SDKMAN_DIR/bin/sdkman-init.sh"
# fi

# if ! command -v fzf 2>&1 >/dev/null
# then
#     eval "$(fzf --zsh)"
# fi

eval "$(fzf --zsh)"
# zsh-defer eval "$(zoxide init zsh)"
eval "$(starship init zsh)"

[[ -d "/opt/nvim-linux64" ]] && export PATH="$PATH:/opt/nvim-linux64/bin"

if type mise &>/dev/null; then
   eval "$(mise activate zsh)"
   mise settings add idiomatic_version_file_enable_tools node
fi
