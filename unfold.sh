#!/bin/sh
# One-time migration from folded stow links (e.g. ~/.config -> repo) to
# file-level links (link.sh uses --no-folding). Safe to re-run.
#   ./unfold.sh -n   show what would happen, change nothing
set -euf # -f: no globbing of paths
R="$(CDPATH= cd "$(dirname "$0")" && pwd -P)"
cd "$R"
N=""
[ "${1:-}" = "-n" ] && N=1
run() { if [ -n "$N" ]; then echo "WOULD: $*"; else echo "$*" && "$@"; fi; }

# Every dir in the package (tracked + untracked), with stow's dot- -> . rename.
DIRS=$(git ls-files -co --exclude-standard | awk -F/ '{p=""; for (i=1; i<NF; i++) {p = p (i>1 ? "/" : "") $i; print p}}' |
    sed -E 's#(^|/)dot-#\1.#g' | LC_ALL=C sort -u)

# 1. Drop whole-dir symlinks that resolve into this repo.
for d in $DIRS; do
    l="$HOME/$d"
    [ -L "$l" ] && [ -d "$l" ] || continue
    case "$(cd "$l" && pwd -P)" in "$R" | "$R"/*) run rm "$l" ;; esac
done

# 2. Move app state that landed in the repo (git-ignored) to its real path in $HOME.
git ls-files -o -i --exclude-standard --directory | grep -vE '^\.claude/$|\.zwc$' | sed 's#/$##' | LC_ALL=C sort |
    while IFS= read -r p; do
        [ -e "$p" ] || [ -L "$p" ] || continue # parent already moved
        case "$p" in "${last:-//}"/*) continue ;; esac  # same, in a dry run
        # Still folded (dry run): ~/$p is the repo copy itself, not a conflict.
        same=$([ "$(cd "$HOME/$(dirname "$p")" 2>/dev/null && pwd -P)" = "$R/$(dirname "$p")" ] && echo 1 || true)
        if [ -z "$same" ] && { [ -e "$HOME/$p" ] || [ -L "$HOME/$p" ]; }; then
            echo "SKIP $p: ~/$p already exists, merge by hand" >&2
            continue
        fi
        run mkdir -p "$HOME/$(dirname "$p")"
        run mv "$p" "$HOME/$p"
        last="$p"
    done

# 3. zsh compile caches next to scripts; zsh regenerates them.
run rm -f scripts/*.zwc

# 4. Relink file by file. Stow aborts everything on conflict, so this is all or nothing.
if [ -n "$N" ]; then
    echo "WOULD: ./link.sh (run ./link.sh -n after the moves to preview)"
else
    ./link.sh -n >/dev/null
    ./link.sh
fi
