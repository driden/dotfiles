#!/bin/bash
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

VERBOSE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --verbose) VERBOSE=1; shift ;;
    --quiet)   VERBOSE=0; shift ;;
    *) break ;;
  esac
done

LOG="$HOME/.local/share/notify-tmux/log"
[ "$VERBOSE" = "1" ] && mkdir -p "$(dirname "$LOG")"
log() {
  [ "$VERBOSE" = "1" ] && echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"
}

raw_args="$*"

# Two payload shapes reach this script:
#   current: <pane_id>
#   legacy : <session> <client_tty> <pane_id>   (notifications posted before the
#            fix linger in Notification Center for days and are still clickable)
# Without this, a legacy click passes a SESSION NAME where a pane id is expected;
# it resolves as a valid target, so the switch silently lands on whatever window
# that session happens to be on instead of the pane that sent the notification.
pane_id=""
if [[ "$1" == %[0-9]* ]]; then
  pane_id="$1"
elif [[ "$3" == %[0-9]* ]]; then
  pane_id="$3"
  log "CLICK legacy payload args=[$raw_args] -> pane='$pane_id'"
fi

if [ -z "$pane_id" ]; then
  log "CLICK unusable payload args=[$raw_args]"
  exit 0
fi

# Resolve the session NOW rather than trusting a value captured when the
# notification was posted. Pane ids are unique server-wide and survive window
# renumbering and renames.
session=$(tmux display-message -p -t "$pane_id" '#S' 2>/dev/null)
if [ -z "$session" ]; then
  log "CLICK pane='$pane_id' no longer exists (args=[$raw_args])"
  exit 0
fi

# Same reasoning for the client: a client tty captured minutes ago is stale the
# moment you detach or reattach, and switch-client then fails with "can't find
# client" and the view never moves. Prefer a client already on the target
# session so we do not yank an unrelated window; otherwise the most recent one.
# What is the attached client actually looking at right now?
client_view() {
  local s
  s=$(tmux list-clients -F '#{client_session}' 2>/dev/null | head -1)
  [ -z "$s" ] && { echo "no-client"; return; }
  tmux display-message -p -t "$s:" '#{session_name}:#{window_index}.#{pane_index}' 2>/dev/null
}

# Pick a client. Two traps here, both of which silently produced an empty
# client_tty (so switch-client never ran and only same-session clicks worked):
#   1. tmux REPLACES a tab separator with "_" when invoked with no $TMUX in the
#      environment, which is exactly the notification-click context. Any
#      tab-delimited -F format parses fine when tested from inside tmux and
#      fails in production. Use a printable multi-char separator instead.
#   2. Parse with bash parameter expansion, not IFS/awk/cut, so no external
#      tool or IFS subtlety can break it.
pick_client() {
  local want="$1" best_tty="" best_act=-1 line act rest tty sess
  while read -r line; do
    [ -z "$line" ] && continue
    act="${line%%:::*}"; rest="${line#*:::}"
    tty="${rest%%:::*}"; sess="${rest#*:::}"
    [ -z "$tty" ] && continue
    [ -n "$want" ] && [ "$sess" != "$want" ] && continue
    case "$act" in (*[!0-9]*|"") act=0 ;; esac
    if [ "$act" -gt "$best_act" ]; then
      best_act="$act"; best_tty="$tty"
    fi
  done < <(tmux list-clients -F '#{client_activity}:::#{client_tty}:::#{client_session}' 2>/dev/null)
  printf '%s' "$best_tty"
}

client_tty=$(pick_client "$session")
[ -z "$client_tty" ] && client_tty=$(pick_client "")

before_view=$(client_view)
sw_status=-
if [ -n "$client_tty" ]; then
  tmux switch-client -c "$client_tty" -t "$session" 2>/dev/null
  sw_status=$?
fi

tmux select-window -t "$pane_id" 2>/dev/null
win_status=$?
tmux select-pane -t "$pane_id" 2>/dev/null

target=$(tmux display-message -p -t "$pane_id" '#S:#I.#P' 2>/dev/null)
after_view=$(client_view)
ok=MOVED-OK; [ "$after_view" != "$target" ] && ok=DID-NOT-LAND
log "CLICK args=[$raw_args] pane='$pane_id' target='$target' view:'$before_view'->'$after_view' client='${client_tty:-none}' switch=$sw_status select=$win_status $ok"
