#!/usr/bin/env bash
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

BLUE='\033[1;34m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
RED='\033[1;31m'
RESET='\033[0m'

log() { printf "\n${BLUE}==> %s${RESET}\n" "$*"; }
ok() { printf "${GREEN}  ✔ %s${RESET}\n" "$*"; }
warn() { printf "${YELLOW}  ! %s${RESET}\n" "$*"; }
die() {
	printf "${RED}ERROR: %s${RESET}\n" "$*" >&2
	exit 1
}

log "Installing Homebrew"

if command -v brew &>/dev/null; then
	ok "Homebrew already installed"
else
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Make brew available in the current shell session
if [[ "$(uname -m)" == "arm64" ]]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
else
	eval "$(/usr/local/bin/brew shellenv)"
fi

ok "Homebrew ready"

# Base tools

log "Installing base tools"
brew install git curl stow zsh
ok "Base tools ready"

# Brewfile packages

log "Installing packages from Brewfile"
brew bundle --file="$DOTFILES_DIR/Brewfile"
ok "Brewfile packages installed"
