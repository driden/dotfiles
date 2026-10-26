-- Rex keymap mirroring ~/.tmux.conf. Prefix is ctrl+b.

-- Splits, vim mnemonics
rex.bind("ctrl+b>v", "pane.split", { direction = "right" }) -- split-window -h
rex.bind("ctrl+b>s", "pane.split", { direction = "down" }) -- split-window -v
rex.bind("ctrl+b>x", "pane.close")
rex.bind("ctrl+b>z", "pane.zoom")
rex.bind("ctrl+b>o", "pane.move_to_tab") -- send pane to another tab

for key, dir in pairs({ h = "left", j = "down", k = "up", l = "right" }) do
	rex.bind("ctrl+b>" .. key, "pane.focus", { direction = dir })
end

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
