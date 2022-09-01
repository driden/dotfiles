local server_list = {
	bashls = {},
	eslint = {},
	tsserver = {},
	gopls = {},
	terraformls = {},
	tflint = {},
	pyright = {},
	sumneko_lua = {
		settings = {
			Lua = {
				runtime = {
					version = "LuaJIT",
					path = vim.split(package.path, ";"),
				},
				diagnostics = { globals = { "vim", "hs" } },
				workspace = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.stdpath("config") .. "/lua"] = true,
					library = vim.api.nvim_get_runtime_file("", true),
					checkThirdParty = false,
				},
				telemetry = { enable = false },
			},
		},
	},
}

local function get_server_names()
	-- There's gotta be a better way
	local server_names_list = {}
	local i = 1
	for key, _ in pairs(server_list) do
		server_names_list[i] = key
		i = i + 1
	end

	return server_names_list
end

return {
	names = get_server_names(),
	list = server_list,
}
