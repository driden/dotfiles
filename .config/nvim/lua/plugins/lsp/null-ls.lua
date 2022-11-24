-- If you are looking for java, then its at ../../ftplugin/

local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics
local code_actions = require("null-ls").builtins.code_actions

require("null-ls").setup({
	debug = false, -- :NullLsLog && :NullLsInfo
	sources = {
		code_actions.eslint_d,
		diagnostics.eslint_d,
		formatting.eslint_d,
		formatting.shfmt,
		formatting.stylua,
	},
	on_attach = require("plugins.lsp.keymaps").on_attach,
})
