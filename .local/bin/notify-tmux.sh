#!/bin/bash
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

VERBOSE=0
if [ "$1" = "--verbose" ]; then
    VERBOSE=1
fi

LOG="$HOME/.local/share/notify-tmux/log"
if [ "$VERBOSE" = "1" ]; then
  mkdir -p "$(dirname "$LOG")"
  # keep the log bounded when it is switched on
  if [ -f "$LOG" ] && [ "$(wc -l < "$LOG")" -gt 400 ]; then
    tail -300 "$LOG" > "$LOG.tmp" && mv "$LOG.tmp" "$LOG"
  fi
fi
log() {
    [ "$VERBOSE" = "1" ] && echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >>"$LOG"
}

input=$(cat)
app=$(printf '%s' "$input" | jq -r '.app // "Claude"')
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
message=$(printf '%s' "$input" | jq -r '.message // empty')
event=$(printf '%s' "$input" | jq -r '.event // .hook_event_name // empty')
session_id=$(printf '%s' "$input" | jq -r '.session_id // empty')
pending_bg=$(printf '%s' "$input" | jq -r '(.background_tasks // []) | length')

if [ "$event" = "Stop" ] && [ "$pending_bg" != "0" ] && [ -n "$pending_bg" ]; then
    log "SKIP event='Stop' pending_bg=$pending_bg (not actually done yet)"
    exit 0
fi

# Identify the pane we are running in. That is the ONLY thing worth capturing
# here: pane ids are unique server-wide and stable, so the click handler can
# re-derive the session and pick a live client at click time.
session=""
label=""
if [ -n "$TMUX_PANE" ]; then
  info=$(tmux display-message -p -t "$TMUX_PANE" '#S'$'\t''#I'$'\t''#W' 2>/dev/null)
  IFS=$'\t' read -r session win_idx win_name <<< "$info"
  if [ -n "$session" ]; then
    label="$session:$win_idx"
    if [ -n "$win_name" ] && [ "$win_name" != "$session" ]; then
      label="$label $win_name"
    fi
  fi
fi

[ -z "$label" ] && label=$(basename "${cwd:-unknown}")

if [ -z "$message" ]; then
    if [ "$event" = "Stop" ]; then
        message="Task finished"
    else
        message="Needs your input"
    fi
fi

# One notification slot PER CLAUDE INSTANCE. Grouping by tmux session name meant
# every Claude sharing a session silently deleted its siblings' notifications,
# so you would click the survivor and land in whichever instance fired last.
group="claude-${session_id:-${TMUX_PANE:-$label}}"

log "HOOK event='$event' pane='$TMUX_PANE' session='$session' label='$label' group='$group' cwd='$cwd'"

click_flag=""
[ "$VERBOSE" = "1" ] && click_flag="--verbose"

args=(-title "$app · $label" -subtitle "$event" -message "$message" -sound default -group "$group")
if [ -n "$session" ] && [ -n "$TMUX_PANE" ]; then
  args+=(-activate "com.mitchellh.ghostty" -execute "$HOME/.local/bin/notify-click.sh $click_flag '$TMUX_PANE'")
fi

terminal-notifier "${args[@]}" >/dev/null 2>&1 || true
