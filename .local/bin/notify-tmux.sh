#!/bin/bash
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

VERBOSE=0
if [ "$1" = "--verbose" ]; then
  VERBOSE=1
fi

LOG="$HOME/.local/share/notify-tmux/log"
log() {
  [ "$VERBOSE" = "1" ] && echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"
}

input=$(cat)
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
message=$(printf '%s' "$input" | jq -r '.message // empty')
event=$(printf '%s' "$input" | jq -r '.hook_event_name // empty')
pending_bg=$(printf '%s' "$input" | jq -r '(.background_tasks // []) | length')

if [ "$event" = "Stop" ] && [ "$pending_bg" != "0" ] && [ -n "$pending_bg" ]; then
  log "SKIP event='Stop' pending_bg=$pending_bg (not actually done yet)"
  exit 0
fi

session=""
if [ -n "$TMUX_PANE" ]; then
  session=$(tmux display-message -p -t "$TMUX_PANE" '#S' 2>/dev/null)
fi

label="${session:-$(basename "${cwd:-unknown}")}"

if [ -z "$message" ]; then
  if [ "$event" = "Stop" ]; then
    message="Task finished"
  else
    message="Needs your input"
  fi
fi

client_tty=$(tmux list-clients -F '#{client_tty}' 2>/dev/null | head -1)

log "HOOK event='$event' session='$session' client_tty='$client_tty' cwd='$cwd' input=$input"

click_flag=""
[ "$VERBOSE" = "1" ] && click_flag="--verbose"

args=(-title "Claude · $label" -subtitle "$event" -message "$message" -sound default -group "claude-$label")
if [ -n "$client_tty" ] && [ -n "$session" ]; then
  args+=(-activate "com.mitchellh.ghostty" -execute "$HOME/.local/bin/notify-click.sh $click_flag '$session' '$client_tty'")
fi

terminal-notifier "${args[@]}" >/dev/null 2>&1
