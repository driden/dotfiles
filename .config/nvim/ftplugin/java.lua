local status, jdtls = pcall(require, "jdtls")
if not status then
	return
end
local capabilities = require("cmp_nvim_lsp").default_capabilities()
capabilities.workspace = { configuration = true }
capabilities.textDocument = {
	completion = {
		completionItem = {
			snippetSupport = true,
		},
	},
}

local extendedClientCapabilities = require("jdtls").extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true

local root_markers = { ".git", "gradlew", "build.gradle", "pom.xml" }
local root_dir = require("jdtls.setup").find_root(root_markers)

-- For Gradle only lets remove the .settings folder
if root_dir ~= nil then
	local f = io.open(root_dir .. "/build.gradle", "r")
	if f ~= nil then
		io.close(f)
		-- vim.g['test#java#runner'] = 'gradletest'
		-- vim.api.nvim_exec(
		-- 	[[
		--          let test#java#runner = 'gradletest'
		--          ]],
		-- 	true
		-- )
		os.execute("rm -rf " .. root_dir .. "/.settings")
	end
end

local home = os.getenv("HOME")

local workspace_dir = home .. "/.local/workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
local on_attach = require("lsp.keymaps").on_attach
local jdtls_dir = vim.fn.stdpath("data") .. "/mason/packages/jdtls"
local plugins_dir = jdtls_dir .. "/plugins"
-- TODO: Find config out by using system's architecture
local config_dir = jdtls_dir .. "/config_mac"
local launcher = vim.fn.glob(plugins_dir .. "/org.eclipse.equinox.launcher_*.jar")

local sdk_dir = home .. "/.sdkman/candidates"
local java_dir = sdk_dir .. "/java/17.0.5-tem"
local java_bin = java_dir .. "/bin/java"

local config = {
	cmd = {
		java_bin,
		string.format("-javaagent:%s", vim.fn.expand(jdtls_dir .. "/lombok.jar")),
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
		"-jar",
		launcher,
		"-data",
		workspace_dir,
		"-configuration",
		config_dir,
	},
	on_attach = on_attach,
	capabilities = capabilities,
	root_dir = root_dir,
	init_options = {
		extendedClientCapabilities = extendedClientCapabilities,
	},
	format = {
		comments = { enabled = false },
		enabled = true,
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
require("jdtls").start_or_attach(config)

vim.cmd(
	"command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
)
vim.cmd(
	"command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
)
vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
