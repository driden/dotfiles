#!/usr/bin/env bash
set -euo pipefail

BLUE=$'\033[1;34m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[1;31m'
RESET=$'\033[0m'

log()  { printf "\n%s==> %s%s\n" "$BLUE" "$*" "$RESET"; }
ok()   { printf "%s  ✔ %s%s\n" "$GREEN" "$*" "$RESET"; }
warn() { printf "%s  ! %s%s\n" "$YELLOW" "$*" "$RESET"; }
die()  { printf "%sERROR: %s%s\n" "$RED" "$*" "$RESET" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

PACMAN_PACKAGES=(
    # Shell & terminal
    zsh
    tmux
    starship

    # File & search utilities
    eza
    fd
    fzf
    ripgrep
    tree

    # Git
    lazygit

    # Editor
    neovim

    # Data & serialization
    jq
    yq

    # Build tools
    cmake

    # JS/TS tooling
    pnpm

    # Dev tooling
    mise

    # Container (Linux-native, rootless)
    podman
    podman-compose

    # Media
    imagemagick
    imv

    # Gaming
    steam
    gamemode
    lib32-gamemode
    wine
    winetricks
    vulkan-icd-loader
    lib32-vulkan-icd-loader

    # System
    speech-dispatcher
    keyd

    # Fonts
    ttf-jetbrains-mono-nerd
    ttf-hack-nerd
)

AUR_PACKAGES=(
    lazydocker          # Docker/Podman TUI
    git-delta           # Better git diffs
    ghostty             # Terminal emulator
    obsidian            # Note taking
    bitwarden           # Password manager
    vesktop             # Discord client
    floorp-bin          # Firefox-based browser
    youtube-music-bin   # YouTube Music desktop client
    heroic-games-launcher-bin  # Epic & GOG client
    mangohud            # In-game performance overlay
    protonup-qt         # Proton-GE version manager
)

install_packages() {
    log "Installing pacman packages"
    sudo pacman -S --noconfirm --needed "${PACMAN_PACKAGES[@]}"
    ok "Pacman packages installed"

    log "Checking for yay"
    if command -v yay &>/dev/null; then
        ok "yay already installed"
    else
        log "Installing yay"
        sudo pacman -S --noconfirm --needed base-devel
        local yay_tmp
        yay_tmp="$(mktemp -d)"
        git clone https://aur.archlinux.org/yay.git "$yay_tmp"
        (cd "$yay_tmp" && makepkg -si --noconfirm)
        rm -rf "$yay_tmp"
        ok "yay installed"
    fi

    log "Installing AUR packages"
    yay -S --noconfirm --needed "${AUR_PACKAGES[@]}"
    ok "AUR packages installed"
}

setup_keyd() {
    log "Setting up keyd"
    sudo mkdir -p /etc/keyd
    sudo cp "$DOTFILES_DIR/keyd/default.conf" /etc/keyd/default.conf
    sudo systemctl enable --now keyd
    ok "keyd enabled — capslock remapped to ctrl/escape"
}

setup_podman() {
    log "Setting up rootless podman"
    systemctl --user enable --now podman.socket
    ok "Podman rootless socket enabled"
}

setup_gamemode() {
    log "Adding $USER to gamemode group"
    sudo usermod -aG gamemode "$USER"
    ok "User added to gamemode group"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

main() {
    install_packages
    setup_keyd
    setup_podman
    setup_gamemode

    log "Post-install complete"
    warn "Log out and back in for group changes (gamemode) to take effect"
}

main
