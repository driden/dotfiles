#!/bin/bash
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

VERBOSE=0
if [ "$1" = "--verbose" ]; then
  VERBOSE=1
  shift
fi

LOG="$HOME/.local/share/notify-tmux/log"
log() {
  [ "$VERBOSE" = "1" ] && echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"
}

session="$1"
client_tty="$2"
pane_id="$3"

tmux switch-client -c "$client_tty" -t "$session"
status=$?

# Pane IDs are unique across the server, so this selects the right window
# even if windows were renumbered. Skip silently if the pane is gone.
win_status=-
if [ -n "$pane_id" ]; then
  if tmux select-window -t "$pane_id" 2>/dev/null; then
    tmux select-pane -t "$pane_id" 2>/dev/null
    win_status=0
  else
    win_status=1
  fi
fi

log "CLICK session='$session' client_tty='$client_tty' pane='$pane_id' switch-client exit=$status select-window exit=$win_status"
exit $status
