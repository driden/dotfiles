-- https://github.com/ChristianChiarulli/nvim/blob/master/lua/user/lsp/handlers.lua
local config = require("lspconfig")
local capabilities = require("cmp_nvim_lsp").default_capabilities()

local list = require("plugins.lsp.servers").list

for server, cfg in pairs(list) do
  local opts = vim.tbl_deep_extend("error", cfg, capabilities)
  config[server].setup {
    on_attach = require("plugins.lsp.keymaps").on_attach,
    capabilities = opts
  }
end

