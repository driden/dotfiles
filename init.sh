#!/usr/bin/env bash
set -euo pipefail

DOTFILES_REPO="git@github.com:driden/dotfiles.git"
DOTFILES_DIR="$HOME/code/dotfiles"
SSH_KEY="$HOME/.ssh/id_driden_gh"

BLUE=$'\033[1;34m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[1;31m'
RESET=$'\033[0m'

log() { printf "\n%s==> %s%s\n" "$BLUE" "$*" "$RESET"; }
ok()  { printf "%s  ✔ %s%s\n" "$GREEN" "$*" "$RESET"; }
warn(){ printf "%s  ! %s%s\n" "$YELLOW" "$*" "$RESET"; }
die() { printf "%sERROR: %s%s\n" "$RED" "$*" "$RESET" >&2; exit 1; }
confirm() {
    printf "\n%s [y/N] " "$1"
    read -r reply
    [[ "$reply" =~ ^[Yy]$ ]]
}

detect_platform() {
    log "Detecting OS"
    local os
    os="$(uname -s)"
    case "$os" in
        Darwin) PLATFORM="macos" ;;
        Linux)
            [ -f /etc/os-release ] || die "Cannot detect Linux distro"
            . /etc/os-release
            case "${ID:-}" in
                ubuntu|debian|*ubuntu*|*debian*) PLATFORM="debian" ;;
                arch|manjaro|*arch*)             PLATFORM="arch" ;;
                fedora|rhel|centos|*fedora*)     PLATFORM="fedora" ;;
                *) die "Unsupported Linux distro: ${ID:-unknown}" ;;
            esac
            ;;
        *) die "Unsupported OS: $os" ;;
    esac
    ok "Platform: $PLATFORM"
}

install_git() {
    log "Installing git"
    if command -v git &>/dev/null; then
        ok "git already installed"
        return
    fi
    case "$PLATFORM" in
        macos)   xcode-select --install ;;
        debian)  sudo apt-get update && sudo apt-get install -y git ;;
        arch)    sudo pacman -Sy --noconfirm git ;;
        fedora)  sudo dnf install -y git ;;
    esac
    ok "git installed"
}

setup_ssh_key() {
    log "SSH key"
    if [ -f "$SSH_KEY" ]; then
        warn "SSH key already exists at $SSH_KEY — skipping generation"
    else
        ssh-keygen -t ed25519 -f "$SSH_KEY" -C "id_driden_gh"
        ok "Generated $SSH_KEY"
    fi

    eval "$(ssh-agent -s)" >/dev/null
    ssh-add "$SSH_KEY"

    printf "\n%sAdd the following public key to GitHub before continuing:%s\n\n" "$YELLOW" "$RESET"
    cat "${SSH_KEY}.pub" | pbcopy
    printf "\nGitHub → Settings → SSH and GPG keys → New SSH key\nAnd paste your key"

    confirm "Have you added the key to GitHub?" || die "Re-run the script after adding the key."
}

clone_dotfiles() {
    log "Cloning dotfiles"
    if [ -d "$DOTFILES_DIR/.git" ]; then
        warn "Dotfiles already cloned at $DOTFILES_DIR — skipping"
    else
        mkdir -p "$(dirname "$DOTFILES_DIR")"
        GIT_SSH_COMMAND="ssh -i $SSH_KEY" git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
        ok "Cloned to $DOTFILES_DIR"
    fi
}

main() {
    detect_platform
    confirm "This will install git, set up your SSH key, and clone your dotfiles. Continue?" || exit 0
    install_git
    setup_ssh_key
    clone_dotfiles

    log "Handing off to bootstrap"
    bash "$DOTFILES_DIR/bootstrap.sh"
}

main
