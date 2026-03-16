#!/usr/bin/env bash
set -euo pipefail

BLUE=$'\033[1;34m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RESET=$'\033[0m'

log()  { printf "\n%s==> %s%s\n" "$BLUE" "$*" "$RESET"; }
ok()   { printf "%s  ✔ %s%s\n" "$GREEN" "$*" "$RESET"; }
warn() { printf "%s  ! %s%s\n" "$YELLOW" "$*" "$RESET"; }

# ─── Dock ────────────────────────────────────────────────────────────────────

setup_dock() {
    log "Configuring Dock"

    # Remove all default apps
    defaults write com.apple.dock persistent-apps -array
    defaults write com.apple.dock persistent-others -array

    # No recent apps in Dock
    defaults write com.apple.dock show-recents -bool false

    # Faster animations
    defaults write com.apple.dock autohide-time-modifier -float 0.2
    defaults write com.apple.dock expose-animation-duration -float 0.1

    # Disable window minimize animation
    defaults write com.apple.dock mineffect -string "scale"
    defaults write com.apple.Dock showhidden -bool true

    killall Dock
    ok "Dock configured"
}

# ─── Keyboard & Text Input ───────────────────────────────────────────────────

setup_keyboard() {
    log "Configuring Keyboard & Text Input"

    # Disable auto-correct
    defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

    # Disable auto-capitalization
    defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

    # Disable smart quotes and smart dashes
    defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
    defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

    # Disable period on double-space
    defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

    ok "Keyboard & Text Input configured"
}

# ─── Finder ──────────────────────────────────────────────────────────────────

setup_finder() {
    log "Configuring Finder"

    # Show hidden files
    defaults write com.apple.finder AppleShowAllFiles -bool true

    # Show full path in titlebar
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

    # Show all file extensions
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true

    # List view as default
    defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

    # Search current folder by default
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

    # Disable the warning when changing a file extension
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    killall Finder
    ok "Finder configured"
}

# ─── Animations ──────────────────────────────────────────────────────────────

setup_animations() {
    log "Disabling animations"

    # Disable opening and closing window animations
    defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

    # Disable smooth scrolling
    defaults write NSGlobalDomain NSScrollAnimationEnabled -bool false

    # Faster Mission Control animations
    defaults write com.apple.dock expose-animation-duration -float 0.1

    # Disable animation when opening applications from the Dock
    defaults write com.apple.dock launchanim -bool false

    # Disable animations when opening/closing sheets and popovers
    defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

    # Disable animation when opening the Info window in Finder
    defaults write com.apple.finder DisableAllAnimations -bool true

    killall Finder
    ok "Animations disabled"
}

# ─── Transparency ─────────────────────────────────────────────────────────────

setup_transparency() {
    log "Disabling transparency"

    # Disable transparency in the menu bar and elsewhere
    sudo defaults write com.apple.universalaccess reduceTransparency -bool true

    ok "Transparency disabled"
}

run_postinstall() {
    local postinstall_script="$DOTFILES_DIR/postinstall/$PLATFORM.sh"

    if [ -f "$postinstall_script" ]; then
        log "Running post-install for $PLATFORM"
        bash "$postinstall_script"
        ok "Post-install complete"
    else
        warn "No post-install script found for $PLATFORM — skipping"
    fi
}

# ─── Main ─────────────────────────────────────────────────────────────────────

main() {
    setup_dock
    setup_keyboard
    setup_finder
    setup_transparency
    warn "Some changes may require a logout or restart to take full effect"
}

main

