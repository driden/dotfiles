#!/usr/bin/env bash

rkt () {
  kitty_dir="$HOME/.config/kitty"
  kitty_themes_dir="$kitty_dir/kitty-themes/themes"

  new_theme=$(ls -al $kitty_themes_dir |  awk '{print $9}' | xargs shuf -n1 -e --)
  echo $new_theme
  rm -f "$kitty_dir/theme.conf" &&
  ln -s "$kitty_themes_dir/$new_theme" "$kitty_dir/theme.conf" &&
  echo "using theme $new_theme"
}
