-- Rex keymap mirroring ~/.tmux.conf. Prefix is ctrl+b.

-- Splits, vim mnemonics
rex.bind("ctrl+b>v", "pane.split", { direction = "right" }) -- split-window -h
rex.bind("ctrl+b>s", "pane.split", { direction = "down" }) -- split-window -v
rex.bind("ctrl+b>x", "pane.close")
rex.bind("ctrl+b>z", "pane.zoom")
rex.bind("ctrl+b>o", "pane.move_to_tab") -- send pane to another tab

-- ctrl+h/j/k/l: move between panes, passing through to Neovim when it is focused.
require("rex-navigator").setup()

-- Tabs. ctrl+shift+h/l have no prefix, like your bind -n C-H/C-L.
rex.bind("ctrl+shift+l", "client.tab.next")
rex.bind("ctrl+shift+h", "client.tab.previous")
-- Session-scoped (client.tab.* walks every session when the tab bar is vertical).
-- The app only follows its own session.select, not a server-side focus change.
local function cycle_window(ctx, step)
	local view = rex.session.view({ session_id = ctx.session_id })
	local windows = view and view.windows or {}
	if #windows < 2 then
		return
	end
	local current = 1
	for i, w in ipairs(windows) do
		if w.window_id == view.active_window_id then
			current = i
		end
	end
	local target = windows[(current - 1 + step) % #windows + 1]
	rex.client.queue("session.select", { session_id = ctx.session_id, window_id = target.window_id })
end
rex.action({
	name = "window_next",
	title = "Next Window in Session",
	category = "Window",
	run = function(ctx)
		cycle_window(ctx, 1)
	end,
})
rex.action({
	name = "window_prev",
	title = "Previous Window in Session",
	category = "Window",
	run = function(ctx)
		cycle_window(ctx, -1)
	end,
})
-- Toggle back to the previous session/tab (tmux last-window, across sessions).
rex.action({
	name = "window_last",
	title = "Last Session/Tab",
	category = "Window",
	run = function(ctx)
		local view = rex.session.view({ session_id = ctx.session_id })
		local res = rex.call("focus_history.reverse", { not_window_id = view and view.active_window_id })
		if res and res.reversed then
			rex.client.queue("session.select", { session_id = res.session_id, window_id = res.window_id })
		end
	end,
})
rex.bind("ctrl+b>shift+6", "window_last") -- ^, like vim's alternate file
rex.bind("ctrl+b>n", "window_next")
rex.bind("ctrl+b>p", "window_prev")
rex.bind("ctrl+b>c", "client.tab.new")
rex.bind("ctrl+b>,", "client.tab.rename")
rex.bind("ctrl+b>shift+7", "client.tab.close") -- &

for i = 1, 9 do
	rex.bind("ctrl+b>" .. i, "client.tab.goto", { index = i })
end

-- Sessions
rex.bind("ctrl+b>d", "session.switch")
rex.bind("ctrl+b>shift+n", "session.new")

rex.bind("ctrl+b>r", "client.config.reload")
rex.bind("ctrl+b>ctrl+b", "pane.send_key", { key = "ctrl+b" })

-- Project picker (haxybaxy/rex-sessionizer), replaces tmux-sessionizer
-- pinned at de86ecd, audited; review `git diff` before pulling. native picker avoids needing Remote Control
require("rex-sessionizer").setup({ work_dir = "~/code", picker = "native" })
rex.bind("ctrl+b>f", "sessionizer")

-- Worktree picker (haxybaxy/rex-worktrunk, needs wt, fzf, jq). Audited at 7465d64; review `git diff` before pulling.
require("rex-worktrunk").setup({ base = "@", picker = "native" })
rex.bind("ctrl+b>w", "worktree") -- g is jira
rex.bind("ctrl+b>shift+w", "worktree_remove")

-- Ports of tmux prefix-a (plannotator sessions) and prefix-g (jira ticket).
rex.action({
	name = "plannotator",
	title = "Open Plannotator Session",
	category = "Session",
	run = require("plannotator"),
})
rex.bind("ctrl+b>a", "plannotator")

rex.action({ name = "jira", title = "Open Jira Ticket", category = "Session", run = require("jira") })
rex.bind("ctrl+b>g", "jira")
