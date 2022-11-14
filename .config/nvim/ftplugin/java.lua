local status, jdtls = pcall(require, "jdtls")
if not status then
  return
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local ws_folders_jdtls = {}

local root_markers = {"packageInfo"}
-- local root_markers = {".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
local root_dir = require("jdtls.setup").find_root(root_markers, "Config")
local home = os.getenv("HOME")

if root_dir then -- root workspace
 local file = io.open(root_dir .. "/.bemol/ws_root_folders")
 if file then
  for line in file:lines() do
   table.insert(ws_folders_jdtls, "file://" .. line)
  end
  file:close()
 end
 else
   return;
end

-- local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
local eclipse_workspace = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
--local workspace_dir = WORKSPACE_PATH .. project_name
local workspace_dir = eclipse_workspace

-- See `:help vim.lsp.start_client` for an overview of the supported `config` options.
local config = {
  cmd = {
    "jdtls",
    "--jvm-arg=-javaagent:" .. home .. "/.local/share/nvim/lsp_servers/jdtls/lombok.jar",
    "-data",
    workspace_dir,
  },
  on_attach = require("plugins.lsp.keymaps").on_attach,
  capabilities = capabilities,
  root_dir = root_dir,
  init_options = {
    workspaceFolders = ws_folders_jdtls,
  },
}

-- This starts a new client & server,
-- or attaches to an existing client & server depending on the `root_dir`.
jdtls.start_or_attach(config)

-- require('jdtls').setup_dap()

vim.cmd(
  "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
)
vim.cmd(
  "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
)
vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
