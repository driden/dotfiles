#!/usr/bin/env bash
set -euo pipefail

DOTFILES_DIR="$HOME/code/dotfiles"
SSH_KEY="$HOME/.ssh/id_driden_gh"
SETUP_DIR="$DOTFILES_DIR/setup"

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

preflight() {
    [ "$(id -u)" -eq 0 ] && die "Do not run this script as root."
    # Only confirm if running standalone, not when called from init.sh
    if [ -z "${BOOTSTRAP_INIT:-}" ]; then
        confirm "This script will set up your machine from scratch. Continue?" || exit 0
    fi
}

run_platform_setup() {
    log "Running platform setup: $PLATFORM"
    bash "$SETUP_DIR/$PLATFORM.sh"

    [ -f "$HOME/.zprofile" ] && source "$HOME/.zprofile" 2>/dev/null || true
    [ -f "$HOME/.profile"  ] && source "$HOME/.profile"  2>/dev/null || true
    export PATH="$HOME/.local/bin:$PATH"
}

install_mise() {
    [ "$PLATFORM" = "macos" ] && return

    log "Installing mise"
    if command -v mise &>/dev/null; then
        ok "mise already installed"
    else
        curl https://mise.run | sh
        export PATH="$HOME/.local/bin:$PATH"
        ok "mise installed"
    fi
}

install_mise_tools() {
    log "Installing mise tools"
    if command -v mise &>/dev/null; then
        mise install --cd "$DOTFILES_DIR"
        ok "mise tools installed"
    else
        warn "mise not found — skipping tool installation"
    fi
}

set_default_shell() {
    log "Setting default shell to zsh"
    local zsh_path
    zsh_path="$(command -v zsh)" || die "zsh not found"

    if [ "$SHELL" = "$zsh_path" ]; then
        ok "zsh is already the default shell"
    else
        if ! grep -qF "$zsh_path" /etc/shells; then
            printf "%s\n" "$zsh_path" | sudo tee -a /etc/shells >/dev/null
        fi
        chsh -s "$zsh_path"
        ok "Default shell changed to zsh — takes effect on next login"
    fi
}

link_dotfiles() {
    log "Linking dotfiles"
    "$DOTFILES_DIR/link.sh"
    ok "Dotfiles linked"
}

install_fonts() {
    log "Installing fonts"
    local font_url="https://drive.google.com/uc?export=download&id=1pt6a93d_XRULz9DBIrRu03ESfSNScbjN"
    local font_tmp
    local font_zip
    font_tmp="$(mktemp -d)"
    font_zip="$font_tmp/ComicCode.zip"

    curl -L "$font_url" -o "$font_zip" || die "Failed to download fonts"
    unzip -o "$font_zip" -d "$font_tmp"

    if [ "$PLATFORM" = "macos" ]; then
        cp "$font_tmp"/**/*.{ttf,otf} "$HOME/Library/Fonts/" 2>/dev/null || true
    else
        mkdir -p "$HOME/.local/share/fonts"
        cp "$font_tmp"/**/*.{ttf,otf} "$HOME/.local/share/fonts/"
        fc-cache -f "$HOME/.local/share/fonts"
    fi

    rm -rf "$font_tmp"
    ok "Fonts installed"
}

summary() {
    printf "%sBootstrap complete!%s\n" "$GREEN" "$RESET"
}

main() {
    detect_platform
    preflight
    run_platform_setup
    install_mise
    install_mise_tools
    set_default_shell
    link_dotfiles
    install_fonts
    summary
}

main
