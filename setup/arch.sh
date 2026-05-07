#!/usr/bin/env bash
set -euo pipefail

BLUE='\033[1;34m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
RED='\033[1;31m'
RESET='\033[0m'

log() { printf "\n%s==> %s%s\n" "$BLUE" "$*" "$RESET"; }
ok() { printf "%s  ✔ %s%s\n" "$GREEN" "$*" "$RESET"; }
warn() { printf "%s  ! %s%s\n" "$YELLOW" "$*" "$RESET"; }
die() {
	printf "%sERROR: %s%s\n" "$RED" "$*" "$RESET" >&2
	exit 1
}

# Base tools
log "Installing base tools"
sudo pacman -Sy --noconfirm git curl stow zsh
ok "Base tools ready"

# Pacman packages
PACMAN_PACKAGES=(
	# Add packages here as needed
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
 
    # Fonts
    ttf-jetbrains-mono-nerd
    ttf-hack-nerd

	
)

log "Installing pacman packages"
if [ ${#PACMAN_PACKAGES[@]} -gt 0 ]; then
	sudo pacman -S --noconfirm "${PACMAN_PACKAGES[@]}"
	ok "Pacman packages installed"
else
	warn "No pacman packages defined — edit setup/arch.sh to add them"
fi

# yay (AUR helper)
log "Installing yay"
if command -v yay &>/dev/null; then
	ok "yay already installed"
else
	sudo pacman -S --noconfirm base-devel
	local_yay_dir="$(mktemp -d)"
	git clone https://aur.archlinux.org/yay.git "$local_yay_dir"
	(cd "$local_yay_dir" && makepkg -si --noconfirm)
	rm -rf "$local_yay_dir"
	ok "yay installed"
fi

# AUR packages
AUR_PACKAGES=(
    lazydocker
    git-delta
    ghostty
    obsidian
    bitwarden
    vesktop
    floorp-bin
    youtube-music-bin
)

log "Installing AUR packages"
if [ ${#AUR_PACKAGES[@]} -gt 0 ]; then
	yay -S --noconfirm "${AUR_PACKAGES[@]}"
	ok "AUR packages installed"
else
	warn "No AUR packages defined — edit setup/arch.sh to add them"
fi
