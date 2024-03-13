vim.api.nvim_buf_create_user_command(0, "Get3Params", function(args)
  local line, _ = unpack(vim.api.nvim_win_get_cursor(0))
  local old = vim.api.nvim_get_current_line()
  local search = "PnsSteps.getDeviceUser(\\(.*\\)\\,\\(.*\\)\\,\\(.*\\))"
  local replace = [[PnsStepsV2.getDeviceUser(
                      \t\tGetUserDevicesRequest.builder()
                      \t\t\t.userId(\1)
                      \t\t\t.predicate(\2)
                      \t\t\t.pageSize(\3)
                      \t\t\t.build())]]
  local lines = vim.split(vim.fn.substitute(old, search, replace, ""), "\n")

  vim.api.nvim_buf_set_lines(0, line - 1, line, false, lines)
end, {})

vim.api.nvim_buf_create_user_command(0, "Get2Params", function(args)
  local line, _ = unpack(vim.api.nvim_win_get_cursor(0))
  local old = vim.api.nvim_get_current_line()
  local search = "PnsSteps.getDeviceUser(\\(.*\\)\\,\\(.*\\))"
  local replace =
  "PnsStepsV2.getDeviceUser(GetUserDevicesRequest.builder()#\\t\\t\\t\\.userId(\\1)#\\t\\t\\t.predicate(\\2)#\\t\\t\\t.build())"
  local lines = vim.split(vim.fn.substitute(old, search, replace, ""), "#")

  vim.api.nvim_buf_set_lines(0, line - 1, line, false, lines)
end, {})

local status, _ = pcall(require, "jdtls")
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

local root_markers = { ".git", "gradlew", "mvnw", "pom.xml" }
local root_dir = require("jdtls.setup").find_root(root_markers)
local home = os.getenv("HOME")

local workspace_dir = home .. "/.local/workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
local on_attach = require("lsp.keymaps").on_attach

local mason_dir = vim.fn.stdpath("data") .. "/mason"
local jdtls_dir = mason_dir .. "/packages/jdtls"
local plugins_dir = jdtls_dir .. "/plugins"
local config_dir = jdtls_dir .. "/config_mac"
local mason_bin_dir = mason_dir .. "/bin"
local launcher = vim.fn.glob(plugins_dir .. "/org.eclipse.equinox.launcher_*.jar")

local sdk_dir = home .. "/.sdkman/candidates"
local java_dir = sdk_dir .. "/java/17.0.6-amzn"
local java_bin = java_dir .. "/bin/java"
-- local lombok_jar = vim.fn.expand(jdtls_dir .. "/lombok.jar")
local lombok_jar = "/Users/lrrezend/Downloads/lombok-1.18.24.jar"

local config = {
  cmd = {
    java_bin,
    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xmx1g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens", "java.base/java.util=ALL-UNNAMED",
    "--add-opens", "java.base/java.lang=ALL-UNNAMED",
    string.format("-javaagent:%s", lombok_jar),
    string.format("-Xbootclasspath/a:%s", lombok_jar),
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
          name = "JavaSE-21",
          path = "$HOME/.sdkman/candidates/java/21.0.2-amzn",
        },
      },
    },
  },
}

config.on_init = function(client, _)
  client.notify("workspace/didChangeConfiguration", { settings = config.settings })
end

require("jdtls").start_or_attach(config)

-- vim.cmd(
--   "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
-- )
-- vim.cmd(
--   "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
-- )
-- vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
-- vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
