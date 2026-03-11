#!/usr/bin/env bash
set -euo pipefail

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

# Base tools

log "Installing base tools"
sudo dnf install -y git curl stow zsh
ok "Base tools ready"

# Packages

log "Installing packages"

PACKAGES=(
)

if [ ${#PACKAGES[@]} -gt 0 ]; then
	sudo dnf install -y "${PACKAGES[@]}"
	ok "Packages installed"
else
	warn "No packages defined — edit setup/fedora.sh to add them"
fi
