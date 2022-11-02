local lua_main = "~/.local/share/nvim/mason/packages/lua-language-server/extension/server/main.lua"

local server_list = {
	bashls = {},
	eslint = {},
	tsserver = {},
	gopls = {},
	terraformls = {},
	tflint = {},
	pyright = {},
	sumneko_lua = {
    cmd = {"lua-language-server","-E", lua_main },
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

return {
	names = vim.tbl_keys(server_list),
	list = server_list,
}
