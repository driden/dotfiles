function add_dir_to_path
    set length (count $argv)
    for dir in $argv
        if test -d $dir
            fish_add_path $dir
        end
    end
end

add_dir_to_path $HOME/bin
add_dir_to_path $HOME/.fzf/bin
add_dir_to_path $HOME/.local/bin
add_dir_to_path $HOME/.local/share/nvim/mason/bin
add_dir_to_path /opt/homebrew/{bin,sbin}
add_dir_to_path $HOME/.cargo/bin
add_dir_to_path $HOME/.ghcup/bin
# add_dir_to_path $HOME/.config/emacs/bin

set TERMINAL wezterm
set EDITOR nvim
set -x MANPAGER "nvim +Man!"
set -x AWS_PAGER ""
set HOMEBREW_NO_AUTO_UPDATE 1

function fish_user_key_bindings
  fish_vi_key_bindings
end

alias v="nvim"

alias zshconfig="nvim ~/.zshrc"
alias refreshenv='source ~/.config/fish/config.fish'
alias dot="cd $HOME/code/dotfiles"
alias ll="eza --color always --long --all"
alias ls="eza --color always"
alias vim=$EDITOR
alias vi=$EDITOR
alias v=$EDITOR
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

fzf --fish | source

set -gx FZF_DEFAULT_COMMAND 'rg --hidden -l ""'
set -gx FZF_ALT_C_COMMAND 'fd --absolute-path --type d --max-depth 2 -E Library -E Pictures -E Music -E Applications -E zsh-plugins -E go . "$HOME"'

# FZF Theme Configuration
if test "$THEME" = "catppuccin"
    set -gx FZF_DEFAULT_OPTS "--layout=reverse --border=rounded --color=bg+:#313244,bg:#1e1e2e,preview-bg:#181825,fg:#cdd6f4,fg+:#cdd6f4,hl:#f38ba8,hl+:#f38ba8,info:#cba6f7,prompt:#cba6f7,pointer:#f5e0dc,marker:#f5e0dc,spinner:#f5e0dc,header:#94e2d5,border:#b4befe"
else
    set -gx FZF_DEFAULT_OPTS "--layout=reverse --border=rounded --color=bg+:#383b35,bg:#252623,preview-bg:#1c1e1b,fg:#f1e9d2,fg+:#f1e9d2,hl:#e75a7c,hl+:#e75a7c,info:#aaaaff,prompt:#8fb573,pointer:#dbb651,marker:#70c2be,spinner:#57a5e5,header:#8fb573,border:#8fb573"
end

set -gx FZF_ALT_C_OPTS "--preview 'eza --icons --color=always --tree --level=1 {}' --preview-window=right:50%:wrap"

mise activate fish | source

if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting # disable welcome message
    starship init fish | source
end
