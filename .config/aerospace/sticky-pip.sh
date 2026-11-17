#!/bin/bash
# Keep Floorp's Picture-in-Picture window on the focused workspace.
pip=$(aerospace list-windows --all --format '%{window-id}|%{app-name}|%{window-title}' |
    awk -F'|' '$2 == "Floorp" && $3 == "Picture-in-Picture" {print $1; exit}')

[[ -n $pip ]] || exit 0

aerospace move-node-to-workspace --window-id "$pip" "$(aerospace list-workspaces --focused)"
