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

tmux switch-client -c "$client_tty" -t "$session"
status=$?
log "CLICK session='$session' client_tty='$client_tty' switch-client exit=$status"
exit $status
