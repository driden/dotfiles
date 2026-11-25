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
rex.bind("ctrl+b>n", "client.tab.next")
rex.bind("ctrl+b>p", "client.tab.previous")
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
