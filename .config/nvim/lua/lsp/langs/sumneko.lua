local list = require("lsp.servers").servers

local setup = function()
	local config = require("lspconfig")
	local capabilities = require("cmp_nvim_lsp").default_capabilities()
	local on_attach = require("lsp.keymaps").on_attach

	config.sumneko_lua.setup({
		on_attach = on_attach,
		capabilities = capabilities,
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
					library = {
						vim.api.nvim_get_runtime_file("", true),
						[vim.fn.expand("$VIMRUNTIME/lua")] = true,
						[vim.fn.stdpath("config") .. "/lua"] = true,
					},
				},
				telemetry = { enable = false },
			},
		},
	})
end

table.insert(list, {"sumneko_lua", setup } )
