#!/bin/sh

DOTFILES_DIR="$(CDPATH= cd "$(dirname "$0")" && pwd)"

# --dry-run / -n : show what would happen without touching anything.
DRY_RUN=""
case "${1:-}" in
    -n | --dry-run) DRY_RUN=1 ;;
    "") ;;
    *) echo "usage: $0 [-n|--dry-run]" >&2; exit 1 ;;
esac
# Skip configs that make no sense on this OS. Same regex rules as
# .stow-local-ignore, but only anchored at the end, so use full paths.
case "$(uname -s)" in
    Darwin) OS_IGNORE='\.config/(awesome|rofi|\.xmobarrc) \.xmonad' ;;
    *) OS_IGNORE='\.config/(aerospace|karabiner|borders|homebrew) \.hammerspoon' ;;
esac

# Per-machine opt-outs: one regex per line in ~/.driden-ignore (not tracked).
LOCAL_IGNORE="$HOME/.driden-ignore"
[ -f "$LOCAL_IGNORE" ] && OS_IGNORE="$OS_IGNORE $(grep -v '^\s*\(#\|$\)' "$LOCAL_IGNORE")"

set -f --  # -f: regexes are not globs
for re in $OS_IGNORE; do set -- "$@" --ignore="$re"; done

# --no-folding: link files, never whole dirs. Otherwise ~/.config itself becomes
# a symlink into this repo and every app's state ends up in git status.
stow --dotfiles --no-folding --stow ${DRY_RUN:+-nv} "$@" --target="$HOME" .

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
