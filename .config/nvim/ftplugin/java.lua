local status, jdtls = pcall(require, "jdtls")
if not status then
	return
end
-- local capabilities = require("cmp_nvim_lsp").default_capabilities()
-- local extendedClientCapabilities = require("jdtls").extendedClientCapabilities
-- extendedClientCapabilities.resolveAdditionalTextEditsSupport = true
--
local capabilities = {
	workspace = {
		configuration = true,
	},
	textDocument = {
		completion = {
			completionItem = {
				snippetSupport = true,
			},
		},
	},
}
local extendedClientCapabilities = require("jdtls").extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true

local root_markers = { ".git", "gradlew", "pom.xml" }
local root_dir = require("jdtls.setup").find_root(root_markers)
local home = os.getenv("HOME")

local workspace_dir = home .. "/.local/workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
-- See `:help vim.lsp.start_client` for an overview of the supported `config` options.
local java_folder = "$HOME/.sdkman/candidates/java/17.0.5-tem"
local java = java_folder .. "/bin/java"
local on_attach = require("lsp.keymaps").on_attach

local server_installation = vim.fn.stdpath("data") .. "/mason/packages/jdtls/plugins"

local launcher = vim.fn.glob(server_installation .. "/org.eclipse.equinox.launcher_*.jar")
local config = {
	cmd = {
		java,
		"-Declipse.application=org.eclipse.jdt.ls.core.id1",
		"-Dosgi.bundles.defaultStartLevel=4",
		"-Declipse.product=org.eclipse.jdt.ls.core.product",
		"-Dlog.protocol=true",
		"-Dlog.level=ALL",
		"-Xmx4g",
		"--add-modules=ALL-SYSTEM",
		"--add-opens",
		"java.base/java.util=ALL-UNNAMED",
		"--add-opens",
		"java.base/java.lang=ALL-UNNAMED",
		"--jvm-arg=-javaagent:" .. home .. "/.local/share/nvim/lsp_servers/jdtls/lombok.jar",
		"-jar",
		launcher,
		"-data",
		workspace_dir,
	},
	on_attach = on_attach,
	capabilities = capabilities,
	root_dir = root_dir,
	init_options = {
		extendedClientCapabilities = extendedClientCapabilities,
	},
	format = {
		comments = { enabled = false },
		enabled = false,
		insertSpaces = true,
		tabSize = 4,
	},
	settings = {
		java = {
			runtimes = {
				{
					name = "JavaSE-17",
					path = "$HOME/.sdkman/candidates/java/17.0.5-tem",
				},
				{
					name = "JavaSE-18",
					path = "$HOME/.sdkman/candidates/java/18.0.1-amzn",
				},
				{
					name = "JavaSE-11",
					path = "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home",
				},
				{
					name = "JavaSE-1.8",
					path = "/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home",
				},
			},
		},
	},
}

config.on_init = function(client, _)
	client.notify("workspace/didChangeConfiguration", { settings = config.settings })
end

vim.pretty_print(config.cmd)
require("jdtls").start_or_attach(config)

local list = require("lsp.servers").servers

local setup = function()
	local config = require("lspconfig")
	local capabilities = require("cmp_nvim_lsp").default_capabilities()
	local on_attach = require("lsp.keymaps").on_attach

	config.jdtls.setup({
		on_attach = on_attach,
		capabilities = capabilities,
	})
end

table.insert(list, { "bashls", setup })

vim.cmd(
	"command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
)
vim.cmd(
	"command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
)
vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
