local M = {}

local function get_lua_opts()
    return {
        settings = {
          Lua = {
            runtime = {version = 'LuaJIT', path = vim.split(package.path, ';')},
            diagnostics = {globals = {'vim'}},
            workspace = {library = vim.api.nvim_get_runtime_file("", true), checkThirdParty = false},
            telemetry = {enable = false}
          }
        }
    }
end

local function setup_server(server)
    local opts = {}
    if server.name == "sumneko_lua" then
      opts = vim.tbl_deep_extend("force", get_lua_opts(), opts)
    end
  server:setup(opts)
end

function M.setup()
  local installer = require'nvim-lsp-installer'
  installer.on_server_ready(setup_server)
    -- This setup() function will take the provided server configuration and decorate it with the necessary properties
    -- before passing it onwards to lspconfig.
    -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
end


return M
