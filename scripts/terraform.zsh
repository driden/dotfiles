#!/usr/bin/env zsh

function tws(){
  local list=$(terraform workspace list)
  local current=$(echo "$list" | awk '$1 == "*" {print $2}')
  local ws=$(echo "$list" | awk '$1 != "*" {print $1}')
  local header=$(printf "Using:%s\nChoose new" "${current}")
  local workspace=$(echo "$ws" | fzf --border --margin=1 --layout=reverse --padding=1 --header "$header")
  terraform workspace select "${workspace}"
}
