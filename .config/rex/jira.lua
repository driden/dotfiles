-- Action body for prefix-g: ask for a ticket ("123" becomes COCAPT-123) and open it.
return function()
	local key = rex.ui.input({ title = "Jira", placeholder = "ticket number or KEY-123" })
	key = key and key:gsub("^%s+", ""):gsub("%s+$", "")
	if not key or key == "" then
		return { cancelled = true }
	end
	local r = rex.exec({ os.getenv("HOME") .. "/work/bin/jira", key })
	if r == nil or not r.ok then
		error("jira: " .. ((r and r.stderr) or "failed to run"), 0)
	end
	return { opened = key }
end
