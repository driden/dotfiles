#!/bin/sh

DOTFILES_DIR="$(CDPATH= cd "$(dirname "$0")" && pwd)"

# --dry-run / -n : show what would happen without touching anything.
DRY_RUN=""
case "${1:-}" in
    -n | --dry-run) DRY_RUN=1 ;;
    "") ;;
    *) echo "usage: $0 [-n|--dry-run]" >&2; exit 1 ;;
esac

stow --dotfiles --stow ${DRY_RUN:+-n} --verbose 2 --target="$HOME" .

# themes/ is not stowed (see .stow-local-ignore); apps read the active theme via
# the ~/.config/themes/current symlink. Point it at the default theme on first
# link, but never clobber an existing choice (switching themes re-points it).
DEFAULT_THEME="bamboo"
THEMES_LINK="$HOME/.config/themes/current"
# Point at the default when nothing usable is linked yet — a valid existing
# choice is left alone, but a missing or broken symlink gets (re)pointed.
# -e is false for both an absent path and a dangling symlink.
if [ ! -e "$THEMES_LINK" ]; then
    if [ -n "$DRY_RUN" ]; then
        echo "WOULD LINK: themes/current => themes/$DEFAULT_THEME"
    else
        mkdir -p "$(dirname "$THEMES_LINK")"
        ln -sfn "$DOTFILES_DIR/themes/$DEFAULT_THEME" "$THEMES_LINK"
        echo "linked themes/current -> $DEFAULT_THEME"
    fi
fi
