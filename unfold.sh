#!/bin/sh
# One-time migration from folded stow links (e.g. ~/.config -> repo) to
# file-level links (install.sh uses --no-folding). Safe to re-run.
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

# 2. Anything not committed doesn't belong in the repo: move it (ignored or not) to
#    its real path in $HOME. Commit what you want to keep in dotfiles first.
git ls-files -o --directory | grep -vE '^\.claude/$|\.zwc$' | sed 's#/$##' | LC_ALL=C sort |
    while IFS= read -r p; do
        [ -e "$p" ] || [ -L "$p" ] || continue # parent already moved
        case "$p" in "${last:-//}"/*) continue ;; esac  # same, in a dry run
        t="$HOME/$(echo "$p" | sed -E 's#(^|/)dot-#\1.#g')" # stow's dot- -> . rename
        if [ "$t" -ef "$p" ]; then
            # ~/... is the repo copy itself: a file-level stow link (drop it) or a
            # still-folded parent (dry run only, step 1 removes those for real).
            [ -L "$t" ] && ! [ "$(dirname "$t")" -ef "$(dirname "$p")" ] && run rm "$t"
        elif [ -e "$t" ] || [ -L "$t" ]; then
            echo "SKIP $p: $t already exists, merge by hand" >&2
            continue
        fi
        run mkdir -p "$(dirname "$t")"
        run mv "$p" "$t"
        last="$p"
    done

# 3. zsh compile caches next to scripts; zsh regenerates them.
run find scripts -maxdepth 1 -name "*.zwc" -delete

# 4. Relink file by file. Stow aborts everything on conflict, so this is all or nothing.
if [ -n "$N" ]; then
    echo "WOULD: ./install.sh (run ./install.sh -n after the moves to preview)"
else
    ./install.sh -n >/dev/null
    ./install.sh
fi
