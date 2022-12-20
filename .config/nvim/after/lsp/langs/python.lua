local list = require("plugins.lsp.servers").servers

local setup = function()
  local config = require("lspconfig")
  local capabilities = require("cmp_nvim_lsp").default_capabilities()
  local on_attach = require("plugins.lsp.keymaps").on_attach

  config.pyright.setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

table.insert(list, { "pyright", setup })
