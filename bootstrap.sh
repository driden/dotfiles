#!/usr/bin/env bash
set -euo pipefail

DOTFILES_REPO="git@github.com:driden/dotfiles.git"
DOTFILES_DIR="$HOME/code/dotfiles"
SSH_KEY="$HOME/.ssh/id_driden_gh"
SETUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/setup" && pwd)"

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
	Darwin)
		PLATFORM="macos"
		;;
	Linux)
		[ -f /etc/os-release ] || die "Cannot detect Linux distro: /etc/os-release not found"
		# shellcheck source=/dev/null
		. /etc/os-release
		case "${ID:-}" in
		ubuntu | debian | *ubuntu* | *debian*) PLATFORM="debian" ;;
		arch | manjaro | *arch*) PLATFORM="arch" ;;
		fedora | rhel | centos | *fedora*) PLATFORM="fedora" ;;
		*) die "Unsupported Linux distro: ${ID:-unknown}" ;;
		esac
		;;
	*)
		die "Unsupported OS: $os"
		;;
	esac

	ok "Platform: $PLATFORM"
}

preflight() {
	[ "$(id -u)" -eq 0 ] && die "Do not run this script as root. Run as a regular user with sudo access."
	confirm "This script will set up your machine from scratch. Continue?" || exit 0
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
	cat "${SSH_KEY}.pub"
	printf "\nGitHub → Settings → SSH and GPG keys → New SSH key\n"

	confirm "Have you added the key to GitHub?" || die "Re-run the script after adding the key."
}

run_platform_setup() {
	log "Running platform setup: $PLATFORM"
	bash "$SETUP_DIR/$PLATFORM.sh"

	# Reload PATH — Homebrew on macOS may have added new entries
	# shellcheck source=/dev/null
	[ -f "$HOME/.zprofile" ] && source "$HOME/.zprofile" 2>/dev/null || true
	# shellcheck source=/dev/null
	[ -f "$HOME/.profile" ] && source "$HOME/.profile" 2>/dev/null || true
	export PATH="$HOME/.local/bin:$PATH"
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
	zsh_path="$(command -v zsh)" || die "zsh not found — cannot set default shell"

	if [ "$SHELL" = "$zsh_path" ]; then
		ok "zsh is already the default shell"
	else
		if ! grep -qF "$zsh_path" /etc/shells; then
			printf "%s\n" "$zsh_path" | sudo tee -a /etc/shells >/dev/null
		fi
		chsh -s "$zsh_path"
		ok "Default shell changed to zsh ($zsh_path) — takes effect on next login"
	fi
}

link_dotfiles() {
	log "Linking dotfiles"
	"$DOTFILES_DIR/link.sh"
	ok "Dotfiles linked"
}

summary() {
	printf "%sBootstrap complete!%s\n" "$GREEN" "$RESET"
}

main() {
	detect_platform
	preflight
	setup_ssh_key
	run_platform_setup
	clone_dotfiles
	install_mise
	install_mise_tools
	set_default_shell
	link_dotfiles
	summary
}

main
