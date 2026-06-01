# Theming strategy — design

**Status:** draft (awaiting user review before plan)
**Date:** 2026-05-30
**Inspiration:** [omarchy themes](https://github.com/basecamp/omarchy/tree/dev/themes/kanagawa)

## Why

The dotfiles currently express the active theme inconsistently across apps:

- `.zshrc` builds `FZF_DEFAULT_OPTS` from an associative array keyed by `$THEME`
- `.config/fish/config.fish` mirrors the same logic with an `if/else`
- `.config/starship.toml` carries `palette = 'bamboo'` plus both bamboo and catppuccin palette tables inline
- `.config/nvim/lua/driden/theme.lua` is a hardcoded Lua string, unrelated to `$THEME`
- `.config/borders/bordersrc` has hardcoded hex
- `.config/aerospace/aerospace.toml` launches `borders` directly with hardcoded hex (a second source of truth for borders colors)
- `.config/ghostty/config` has no color directives at all
- `.tmux.conf` lines 44–64 are hardcoded bamboo hex

To add or switch a theme today, a half-dozen unrelated files must be edited by hand, and `bamboo` is the only theme that has every file filled in. Inspired by omarchy's per-theme directory layout, this design replaces that scatter with a single source of truth per theme and a CLI that switches all participating apps live.

## Goals

- One directory per theme. Adding a theme means creating one directory.
- One CLI: `theme set <name>`, `theme list`, `theme current`, `theme build`, `theme init <name>`.
- Live switching for all five v1 apps: fzf (in running shells), starship, ghostty, jankyborders, tmux, neovim.
- Auto-generate as much per-app config as possible from a small palette; allow per-theme hand-written overrides where palette-alone is insufficient (today: neovim only).
- Bootstrap leaves the machine on a known theme (`bamboo`) as the last step of `postinstall/{macos,arch}.sh`.

## Non-goals (v1)

- Wallpaper switching.
- Hyprland, alacritty, kitty, wezterm — out of scope until used as the primary tool.
- Light themes. Schema includes `appearance = "dark" | "light"` for forward compatibility but v1 ships only dark themes.
- A custom DSL or templating engine. Templates use `${palette.NAME}` string substitution only.

## v1 scope

**Apps:** neovim, ghostty, jankyborders, fzf, starship, tmux.
**Themes:** `bamboo` (current), `kanagawa` (new).

## Architecture

### Directory layout

```
themes/
  bamboo/
    colors.toml          # palette (source of truth)
    name                 # one-line file containing "bamboo" — nvim reads this
    nvim.lua             # hand-written: plugin name + setup opts (per-theme)
    starship.toml        # generated from templates/starship.toml.tmpl
    ghostty.conf         # generated
    borders.sh           # generated
    fzf.sh               # generated
    tmux.conf            # generated
  kanagawa/
    colors.toml
    name
    nvim.lua
    starship.toml        # generated
    ghostty.conf         # generated
    borders.sh           # generated
    fzf.sh               # generated
    tmux.conf            # generated
  templates/
    starship.toml.tmpl
    ghostty.conf.tmpl
    borders.sh.tmpl
    fzf.sh.tmpl
    tmux.conf.tmpl
```

Generated files are **committed**, not regenerated at switch time. `theme set` is template-free and only flips a symlink + signals apps. `theme build` is the meta-command run when `colors.toml` or a template changes; you eyeball the `git diff` of generated files and commit them.

### The `current` symlink

```
~/.config/themes/current  ──►  ~/code/dotfiles/themes/<active_theme>
```

This symlink is the only thing `theme set` mutates. Every app's config reaches its themed file through this stable path (`~/.config/themes/current/<file>`).

Created by `theme set` on first run (or by bootstrap). Not stowed.

### How `colors.toml` gets populated

You don't author 22 hex values by hand. The primary path is extraction from the matching Neovim colorscheme via `theme extract <nvim-colorscheme>` (see CLI below), which spawns headless nvim, loads the colorscheme, and emits a `colors.toml`. The extractor reads:

| Target key | Source | Fallback chain |
|---|---|---|
| `color0` … `color15` | `g:terminal_color_0` … `g:terminal_color_15` | none — required; if missing, the extractor errors |
| `background` | `Normal.bg` | none — required |
| `foreground` | `Normal.fg` | none — required |
| `selection_background` | `Visual.bg` | `Normal.bg` |
| `selection_foreground` | `Visual.fg` | `Normal.fg` |
| `cursor` | `Cursor.bg` | `Normal.fg` (`Cursor` is frequently unset by colorschemes) |
| `accent` | `color5` by default | colorscheme has no canonical "signature accent" highlight group; the extractor picks `color5` as a convention, but **`accent` is always intended to be hand-tweaked in the resulting `colors.toml`** |

A **dry-run validation** of this approach against `bamboo.nvim` was performed (2026-05-30):

- 14 of 16 ANSI slots matched the ground-truth bamboo hex scattered across the existing `.zshrc`, `.tmux.conf`, `bordersrc`, and `starship.toml` exactly.
- `selection_background` drifted by 2 RGB units (`#3a3d37` from extraction vs `#383b35` in the legacy fzf array) — bamboo.nvim has been revised since the hex was copied into the dotfiles. Trust the extraction.
- `Cursor` was nil — the documented fallback (`Normal.fg`) covers it.
- `accent`: extractor's default of `color5` happened to be `#aaaaff` (lavender) for bamboo, which matches bamboo's signature in `bordersrc` and fzf's `info`. For other themes this won't be guaranteed; eyeball and override.
- Colors that appear in the legacy dotfiles but **not** in bamboo.nvim's standard palette (`#ff9966` peach, `#f08080` flamingo) are dropped from `colors.toml`. If a template needs an extra color slot, the schema's `[palette.extras]` table (read by hand-written `nvim.lua` / `starship.toml` only, never by generated templates) is the escape hatch.

If you ever add a colorscheme that doesn't set `g:terminal_color_*` at all, the extractor will fail loudly and you'll fall back to hand-authoring `colors.toml`. A "derive ANSI from highlight groups" heuristic is **not** implemented in v1 (YAGNI — none of the themes you currently use need it).

### `colors.toml` schema

22 keys total, modeled after omarchy. Standard X11 terminal vocabulary:

```toml
[meta]
name = "bamboo"
appearance = "dark"        # "dark" | "light" — informs template choices; v1 = dark only

[palette]
accent               = "#aaaaff"
cursor               = "#aaaaff"
foreground           = "#f1e9d2"
background           = "#252623"
selection_foreground = "#f1e9d2"
selection_background = "#383b35"

color0  = "#252623"   # black
color1  = "#e75a7c"   # red
color2  = "#8fb573"   # green
color3  = "#dbb651"   # yellow
color4  = "#57a5e5"   # blue
color5  = "#e75a7c"   # magenta
color6  = "#70c2be"   # cyan
color7  = "#f1e9d2"   # white
color8  = "#383b35"   # bright black
color9  = "#f08080"   # bright red
color10 = "#8fb573"   # bright green
color11 = "#dbb651"   # bright yellow
color12 = "#57a5e5"   # bright blue
color13 = "#e75a7c"   # bright magenta
color14 = "#70c2be"   # bright cyan
color15 = "#f1e9d2"   # bright white
```

Themes with richer native palettes (e.g., kanagawa's `waveBlue`, `dragonRed`) are free to use those in their hand-written `nvim.lua`; they only need to honor these 22 slots for the templated files.

`theme build` fails loudly if any key in `[palette]` is missing.

### Templates

Pure `${palette.NAME}` substitution. No conditionals. Renderer is a ~30-line bash sed loop inside the `theme` CLI — no templating dependency.

The hex strings the templates emit may need transformation for some apps (e.g., jankyborders wants `0xff<hex>` without the leading `#`). That stripping is also done by sed in the renderer.

### Per-app integration

| App | Integration | Switch trigger |
|---|---|---|
| **starship** | `~/.config/starship.toml` is a symlink (created by `theme set`) → `~/.config/themes/current/starship.toml`. The template renders the same prompt structure for every theme; only the `[palettes.theme]` table differs. | Next prompt reads the new file. No signal needed. |
| **fzf** | `.zshrc` and `config.fish` `source ~/.config/themes/current/fzf.sh`, which exports `FZF_DEFAULT_OPTS`. A `precmd` hook (zsh) / `fish_prompt` event (fish) re-sources it on every prompt so running shells pick up changes without restart. | Next prompt re-sources. |
| **ghostty** | `~/.config/ghostty/config` adds `config-file = ~/.config/themes/current/ghostty.conf`. The themed file sets `background`, `foreground`, `cursor-color`, `selection-*`, and `palette = 0..15`. | `pkill -SIGUSR2 ghostty` — ghostty's documented config-reload signal. Does **not** kill the process; equivalent to pressing the bound `super+alt+r` in every window. Includes the window running `theme set` (acceptable cost). |
| **jankyborders** | `bordersrc` is rewritten to `source ~/.config/themes/current/borders.sh` (sets `BORDERS_ACTIVE_COLOR`, `BORDERS_INACTIVE_COLOR`), then invokes `borders` with those vars. `aerospace.toml:13` changes from inline `exec-and-forget borders width=...` to `exec-and-forget /Users/driden/.config/borders/bordersrc` so there's a single launch path. | `pkill borders; nohup bordersrc >/dev/null 2>&1 &`. Aerospace uses `exec-and-forget` (untracked), so killing borders does not respawn it via aerospace; the script must re-exec `bordersrc`. |
| **tmux** | `.tmux.conf` lines 44–64 are deleted and replaced with `source-file ~/.config/themes/current/tmux.conf`. The themed file contains every `set -g status-*`, `window-status-*`, `pane-*`, `message-*`, `mode-style` directive. | `tmux ls -F '#{socket_path}' \| sort -u \| xargs -I{} tmux -S {} source-file ~/.tmux.conf` — applied across every running session. |
| **neovim** | `.config/nvim/lua/driden/theme.lua` reads `~/.config/themes/current/name` at startup. `.config/nvim/lua/plugins/themes.lua` installs **all** supported theme plugins unconditionally (no `enabled = theme == "X"` gating, no `config = function() vim.cmd.colorscheme(...)` side effect at load). `driden.theme.apply()` runs after lazy.nvim finishes loading and sets the colorscheme based on the file's contents. | `for s in $XDG_RUNTIME_DIR/nvim.*; do nvim --server "$s" --remote-send '<C-\><C-N>:lua require("driden.theme").reload()<CR>'; done`. Stale sockets fail silently. |

### The `theme` CLI

Lives at `.local/bin/theme` (stowed to `~/.local/bin/theme`). Single bash script. Subcommands:

```sh
theme list
    # Print available themes (directories under themes/), mark the current one.

theme current
    # Print the active theme name (resolves the current symlink).

theme set <name>
    # 1. Validate themes/<name>/ exists.
    # 2. Run `ln -sfn $REPO/themes/<name> ~/.config/themes/current`.
    # 3. Ensure ~/.config/starship.toml is a symlink to ~/.config/themes/current/starship.toml
    #    (idempotent: relink if needed).
    # 4. Apply live updates to running apps (see table above). Each step is
    #    independent — failure of one (e.g., ghostty not running) does not
    #    abort the others.
    # 5. Print summary: which apps were signaled, which were skipped.

theme build [<name>]
    # Render templates → per-app files for the named theme, or all themes
    # if no name given. Validates colors.toml. Writes ghostty.conf, borders.sh,
    # fzf.sh, tmux.conf, starship.toml into themes/<name>/.

theme build --check
    # Render to temp files; exit non-zero if outputs differ from committed files.
    # For sanity-checking that generated files are in sync with templates and
    # palette.

theme init <name> [--from <nvim-colorscheme>]
    # Scaffold themes/<name>/:
    #   - colors.toml — populated by `theme extract <nvim-colorscheme>` when
    #     --from is given; otherwise every required key with placeholder hex
    #     ("#000000") and a top-comment listing slots to fill
    #   - name file containing "<name>"
    #   - nvim.lua stub with a TODO comment
    # Then runs `theme build <name>` so the generated files exist.

theme extract <nvim-colorscheme> [--name <name>] [--write]
    # Spawn headless nvim, load the colorscheme, dump a colors.toml.
    # Without --write: prints to stdout.
    # With --write and --name X: creates themes/X/colors.toml, then runs
    # `theme build X`.
    # Errors if `g:terminal_color_0..15` is missing — the schema requires
    # all 16 ANSI slots.
```

The extractor's headless invocation handles the case where the colorscheme's plugin isn't on the runtime path of the headless instance: it locates the plugin under `~/.local/share/nvim/lazy/<plugin>` (or the equivalent for other plugin managers) and prepends it to `rtp` before running `:colorscheme`. If multiple plugins might provide the colorscheme, the user can pass `--plugin-dir <path>` to disambiguate.

### File changes

**New files:**

```
themes/bamboo/colors.toml
themes/bamboo/name
themes/bamboo/nvim.lua
themes/bamboo/starship.toml          (generated, committed)
themes/bamboo/ghostty.conf           (generated, committed)
themes/bamboo/borders.sh             (generated, committed)
themes/bamboo/fzf.sh                 (generated, committed)
themes/bamboo/tmux.conf              (generated, committed)
themes/kanagawa/colors.toml
themes/kanagawa/name
themes/kanagawa/nvim.lua
themes/kanagawa/starship.toml        (generated)
themes/kanagawa/ghostty.conf         (generated)
themes/kanagawa/borders.sh           (generated)
themes/kanagawa/fzf.sh               (generated)
themes/kanagawa/tmux.conf            (generated)
themes/templates/starship.toml.tmpl
themes/templates/ghostty.conf.tmpl
themes/templates/borders.sh.tmpl
themes/templates/fzf.sh.tmpl
themes/templates/tmux.conf.tmpl
.local/bin/theme                     (the CLI script)
```

**Modified files:**

```
.zshrc
    - Remove fzf_bamboo_colors / fzf_catppuccin_colors associative arrays
    - Remove _fzf_build_colors function
    - Remove FZF_THEME_COLORS / FZF_DEFAULT_OPTS construction
    - Remove `export THEME="bamboo"` line
    - Add: source ~/.config/themes/current/fzf.sh
    - Add: precmd hook that re-sources the file each prompt

.config/fish/config.fish
    - Remove the if/else block that sets FZF_DEFAULT_OPTS
    - Add: source ~/.config/themes/current/fzf.sh
    - Add: fish_prompt event handler that re-sources the file

.tmux.conf
    - Delete lines 44–64 (the bamboo-colored block)
    - Add: source-file ~/.config/themes/current/tmux.conf

.config/ghostty/config
    - Add: config-file = ~/.config/themes/current/ghostty.conf

.config/borders/bordersrc
    - Rewrite to source ~/.config/themes/current/borders.sh and
      pass $BORDERS_ACTIVE_COLOR / $BORDERS_INACTIVE_COLOR to the
      borders command.

.config/aerospace/aerospace.toml
    - Change line 13 from inline `borders ...` to
      `exec-and-forget /Users/driden/.config/borders/bordersrc`.

.config/nvim/lua/driden/theme.lua
    - Rewrite to read ~/.config/themes/current/name at runtime.
    - Provide M.current(), M.apply(), M.reload().
    - apply() maps theme name → colorscheme string (handles variants
      like kanagawa → "kanagawa-dragon").

.config/nvim/lua/plugins/themes.lua
    - Remove `enabled = theme == "X"` gating from every plugin spec.
    - Remove `config = function() vim.cmd.colorscheme(...) end` side effects.
    - All theme plugins install unconditionally; setup-only configs remain
      (e.g., gruvbox's italic options).

.config/nvim/init.lua
    - Add (near the bottom, after plugins load):
        require("driden.theme").apply()

.config/starship.toml
    - DELETED from the repo. Replaced by themes/<name>/starship.toml (generated)
      and a runtime symlink ~/.config/starship.toml created by `theme set`.

postinstall/macos.sh
    - At the end: `theme set bamboo`.

postinstall/arch.sh
    - At the end: `theme set bamboo`.

README.md
    - Add a "Theming" section (see below).

.stow-local-ignore
    - Add: `themes` (the themes/ directory should not be symlinked into $HOME).
    - Add: `docs` if not already ignored (these design docs shouldn't be stowed).
```

### Bootstrap integration

`theme set bamboo` runs as the **last** step of `postinstall/{macos,arch}.sh`. By that point:

- Stow has run, so `~/.local/bin/theme` exists.
- All apps are installed.
- All config files that reference the symlink (`~/.zshrc`, `.tmux.conf`, etc.) exist at their stowed locations.

The first `theme set` creates `~/.config/themes/current` and `~/.config/starship.toml` symlinks.

## Failure modes

| Scenario | Behavior |
|---|---|
| `theme set <unknown>` | Validation step fails, no symlink change, exit non-zero. |
| `~/.config/themes/current` missing on first run | `theme set <name>` creates it. Other apps fail to find their themed files until then; `postinstall` covers this on bootstrap. |
| App not running when signaled | `pkill` returns non-zero; `theme set` ignores and continues with other apps. |
| nvim socket present but instance has exited | `nvim --server ... --remote-send` fails; `theme set` continues. Sockets are not cleaned up here. |
| `theme build --check` detects drift | Exits non-zero; user runs `theme build` to regenerate, eyeballs `git diff`, commits. |
| `colors.toml` missing a required key | `theme build` fails with the missing key name, refuses to write output files. |

## README addition

Append to `README.md`:

```markdown
## Theming

Manage and switch themes across neovim, ghostty, jankyborders, fzf, starship, and tmux.

### Commands

| Command | What it does |
|---|---|
| `theme list` | Print available themes; mark the current one |
| `theme current` | Print the active theme name |
| `theme set <name>` | Switch live: flip the `themes/current` symlink and signal running apps |
| `theme build [<name>]` | Render templates → per-app files for one theme or all themes |
| `theme build --check` | Verify generated files match what templates would produce |
| `theme init <name>` | Scaffold a new theme directory |

### Adding a new theme

If a matching neovim colorscheme exists (most common):

1. Install the nvim plugin as usual (`.config/nvim/lua/plugins/themes.lua`).
2. `theme init my-theme --from <nvim-colorscheme>` — extracts colors from the plugin via headless nvim and writes `themes/my-theme/colors.toml`, then runs `theme build my-theme`.
3. Eyeball `themes/my-theme/colors.toml`. The `accent` slot defaults to `color5` — adjust if you want a different signature color.
4. Edit `themes/my-theme/nvim.lua` to point at the right plugin/variant.
5. Commit.
6. `theme set my-theme` — apply it live.

If you're authoring a palette from scratch (no matching nvim colorscheme):

1. `theme init my-theme` — creates `themes/my-theme/` with placeholder hex.
2. Edit `themes/my-theme/colors.toml` — fill in all 22 palette keys by hand.
3. `theme build my-theme`.
4. Eyeball the `git diff`. Commit.
5. `theme set my-theme`.

### Tweaking colors or templates

1. Edit `themes/<name>/colors.toml` or any template under `themes/templates/`.
2. `theme build` (no args = all themes).
3. Eyeball the `git diff` of generated files. Commit.
```

## Open items deferred to plan

- Exact bash for the renderer (string substitution + the `#`-stripping for borders hex).
- Concrete content of `templates/tmux.conf.tmpl` (deriving it from the current `.tmux.conf` lines 44–64).
- The exact colorscheme-name mapping table inside `driden.theme.apply()` (kanagawa → `kanagawa-dragon`, etc.).
- Kanagawa's `colors.toml` values — produced by running `theme extract kanagawa --name kanagawa --write` once the CLI is built.
- Fish's per-prompt re-source mechanism (fish doesn't have `precmd` — use `fish_prompt` event).
