#!/bin/env sh

# Homebrew

brew install git neovim kitty fzf lazygit

# Setup

# Zsh plugins

## Vim Mode
git clone https://github.com/jeffreytse/zsh-vi-mode \
  $ZSH/custom/plugins/zsh-vi-mode

#fix <C-R> with Vi mode
