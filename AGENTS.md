# AGENTS.md

This file provides guidance to coding agents (Claude Code, etc.) when working with code in this repository.

## What this repo is

A personal dotfiles repo deployed to `$HOME` via GNU Stow. Files in this repo are symlinked into the home directory — editing a tracked file here changes the live config. The repo also doubles as a from-scratch machine provisioner (macOS, Arch, Debian, Fedora).

## Common commands

```sh
./link.sh              # stow --dotfiles --stow → symlinks everything into $HOME,
                       #   then points ~/.config/themes/current at the default theme
./link.sh --dry-run    # (-n) show what stow + the theme symlink WOULD do, change nothing
./unlink.sh            # stow --dotfiles --delete → removes those symlinks
./init.sh              # step 1/2 for a brand-new machine: ssh key + clone + hand off to bootstrap.sh
./bootstrap.sh         # step 2/2: platform setup → mise tools → default shell → stow → fonts → postinstall
```

`stow --dotfiles` strips the `dot-` prefix from filenames, so a tracked `dot-zshrc` would land as `~/.zshrc`. Tracked dotfiles in this repo already start with `.` and stow handles them directly.

## Layout that matters

- `bootstrap.sh` / `init.sh` — provisioning entry points. `init.sh` is for empty machines (needs git + ssh); `bootstrap.sh` can run on its own once the repo exists.
- `setup/{macos,arch,debian,fedora}.sh` — installs packages for that platform. macOS reads `Brewfile`; Arch keeps `PACMAN_PACKAGES` and `AUR_PACKAGES` arrays inline.
- `postinstall/{macos,arch}.sh` — OS-level tweaks run after packages are in place (macOS uses `defaults write` for Dock/Finder/keyboard/etc.).
- `.config/mise/config.toml` — runtime versions managed by mise (go, node, java, lua 5.1, bun, ruff, uv, shellcheck, shfmt, stylua, lua-language-server, opencode, pi). On macOS, mise itself is installed via brew (so `install_mise` in bootstrap is a no-op there); Linux installs it from the upstream installer.
- `.stow-local-ignore` — files Stow must NOT symlink (scripts, README, setup dirs, etc.). When adding top-level repo files that shouldn't land in `$HOME`, add them here.
- `scripts/` — sourced by `.zshrc` from `$HOME/scripts` (and `$HOME/workscripts` if present). Anything matching `*.zsh` or `*.sh` is auto-sourced.
- `.local/bin/` — user-scope executables stowed to `~/.local/bin/`. Includes `tmux-sessionizer` (custom), `theme`, and several `build-*` scripts.
- `.config/nvim/plugins/git-perma.nvim/` — a local Neovim plugin lives inside the config; `init.lua` loads it via `dir = vim.fn.stdpath("config") .. "/plugins/git-perma.nvim"`.

## Adding a new tool

- **macOS package** → add to `Brewfile`.
- **Arch package** → append to `PACMAN_PACKAGES` (official) or `AUR_PACKAGES` (yay) in `setup/arch.sh`.
- **Cross-platform managed runtime** (anything with a versioned toolchain) → `.config/mise/config.toml`.
- **Top-level repo file that must NOT be stowed** → add to `.stow-local-ignore` before running `link.sh`.

## Stow ignore gotcha

`.stow-local-ignore` entries are **anchored Perl regexes** (full-match), tested against *both* the basename *and* the package-relative path of every node — not globs.

- A bare word like `rofi` matches the basename, so it would ignore **every** `rofi` dir — including `.config/rofi`. To ignore only a nested path, write the full path (`.config/git/ignore`).
- Anchoring is exact: a typo like `postinstal` does **not** match `postinstall`, so the entry silently does nothing and the dir gets stowed anyway. Verify with `./link.sh --dry-run`.

## Neovim specifics

- Plugin manager is `lazy.nvim`; plugin specs live in `.config/nvim/lua/plugins/` and are pulled in via `{ import = "plugins" }` in `init.lua`.
- Parsers are managed by `romus204/tree-sitter-manager.nvim` (auto-install on first edit of a new filetype) alongside `nvim-treesitter/nvim-treesitter` — the system `tree-sitter` CLI must be installed.
- `conform.nvim` does format-on-save but has an explicit `disable_filetypes` list (c, cpp, java, kotlin, typescript, python, html). To enable a formatter for one of these, both remove it from `disable_filetypes` AND add it to `formatters_by_ft`.
- `init.lua` requires `usercommands` at the bottom; user commands live in `lua/usercommands.lua`.

## Theming

Themes live in `themes/<name>/` — each dir has `colors.toml` (semantic palette) plus generated outputs. **Only `starship.toml` is wired up right now** (`borders.sh`/`fzf.sh`/`tmux.conf` exist in some dirs but are not yet built or consumed — WIP). `themes/` is **not** stowed (it's in `.stow-local-ignore`); the active theme lives at the `~/.config/themes/current` symlink, which `link.sh` creates pointing at `bamboo` on first link (never clobbering an existing choice). Switch with `theme set <name>`, which **only** repoints that symlink — nothing is reloaded/hooked yet.

**CLI** (`.local/bin/theme`, stowed to `~/.local/bin/`):
```sh
theme list                                  # list themes, * = active
theme current                               # print the active theme name
theme set <name>                            # repoint ~/.config/themes/current at <name> (symlink only)
theme build [<name>]                        # render templates → per-theme output files (all themes if no name)
theme build --check                         # fail if any generated file drifts from its template
theme init <name> [--from <nvim-scheme>]    # scaffold a theme dir (optionally seed from nvim)
theme extract <nvim-scheme> [--name <name>] [--plugin-dir <path>]   # extract palette from nvim + build
```

**Semantic palette** — `colors.toml` has 20 roles in three groups:
- Chrome: `background foreground cursor selection_background selection_foreground`
- Syntax: `comment keyword string function type number variable constant operator property parameter`
- Diagnostics: `error warning info hint`

**Templates** in `themes/templates/` use `${palette.ROLE}` substitution. `theme build` currently renders **only** `starship.toml.tmpl` → `starship.toml`. The other templates (`borders.sh.tmpl`, `fzf.sh.tmpl`, `tmux.conf.tmpl`) exist but are intentionally not built yet; add them back to the `pairs` lists in `.local/bin/theme` when they're ready.

**Opting out of regeneration:** drop a `.do-not-build` file in a theme dir to mark it hand-maintained. `theme build`, `theme build <name>`, and `theme build --check` all skip that dir (no overwrite, no false drift). Delete the marker to let templates regenerate it again. `cyberdream` is hand-maintained this way.

**Gotchas:**
- `extract.lua` always emits `appearance = "dark"` — manually fix for light themes (e.g. kanagawa-lotus).
- Starship v1.25.1 does not support multi-file `STARSHIP_CONFIG` or native includes.
- Colorscheme plugins are auto-located via `~/.local/share/nvim/lazy/`; use `--plugin-dir` to override.

## Shells

zsh is the intended interactive shell (`bootstrap.sh` chsh's to it). `.zshrc` bootstraps Znap (`zsh-plugins/`) on first run and sources `chitoku-k/fzf-zsh-completions`, `zsh-syntax-highlighting`, `zsh-autosuggestions`. fish config exists and is kept in parity for aliases/paths but isn't the default.
