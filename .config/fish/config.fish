function add_dir_to_path
    set length (count $argv)
    for dir in $argv
        if test -d $dir
            fish_add_path $dir
        end
    end
end

add_dir_to_path $HOME/.bin
add_dir_to_path $HOME/.local/bin
add_dir_to_path $HOME/.local/share/nvim/mason/bin
add_dir_to_path $HOME/neovim/bin
add_dir_to_path $HOME/.config/emacs/bin
add_dir_to_path /opt/homebrew/{bin,sbin}
add_dir_to_path $HOME/.cargo/bin
add_dir_to_path $HOME/.ghcup/bin
add_dir_to_path /opt/nvim-linux64/bin

set TERMINAL wezterm
set EDITOR nvim
set -x MANPAGER "nvim +Man!"
set -x AWS_PAGER ""

function fish_user_key_bindings
  fish_vi_key_bindings
end


alias zshconfig="nvim ~/.zshrc"
alias refreshenv='source $HOME/.zshrc && source $HOME/.zshenv'
alias dot="cd $HOME/code/dotfiles && $EDITOR ."
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
zoxide init fish | source
fnm env --use-on-cd --fnm-dir $HOME/.local/share/fnm | source

if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting # disable welcome message
    # starship init fish | source
end
