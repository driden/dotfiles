-- Action body for prefix-a: pick a live plannotator session in the command
-- palette and open it. Rows come from `plannotator-pick --list`
-- (project, label, age, url).

-- argv through a login shell, so mise tools and homebrew are on PATH.
local function login(argv)
	local cmd = { os.getenv("SHELL") or "/bin/zsh", "-lc", 'exec "$@"', "rex-login" }
	for _, a in ipairs(argv) do
		cmd[#cmd + 1] = a
	end
	return cmd
end

return function()
	local r = rex.exec(login({ os.getenv("HOME") .. "/.local/bin/plannotator-pick", "--list" }), { timeout = 10 })
	if r == nil or not r.ok then
		error("plannotator: " .. ((r and r.stderr) or "failed to run"), 0)
	end
	local items = {}
	for line in (r.stdout or ""):gmatch("[^\n]+") do
		local project, label, age, url = line:match("^([^\t]*)\t([^\t]*)\t([^\t]*)\t(.+)$")
		if url then
			items[#items + 1] = { id = url, title = project .. " · " .. label, detail = age, icon = "globe" }
		end
	end
	if #items == 0 then
		error("no live plannotator sessions", 0)
	end
	local url = items[1].id
	if #items > 1 then
		local answer = rex.ui.pick({ title = "Plannotator", placeholder = "Pick a session", items = items })
		if answer == nil then
			return { cancelled = true }
		end
		url = answer
	end
	rex.exec({ "open", url })
	return { opened = url }
end
