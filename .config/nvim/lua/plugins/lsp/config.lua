-- https://github.com/ChristianChiarulli/nvim/blob/master/lua/user/lsp/handlers.lua

local config = require("lspconfig")
local capabilities = vim.lsp.protocol.make_client_capabilities()

capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

config['tsserver'].setup { capabilities = capabilities }
config['bashls'].setup { capabilities = capabilities }
config['eslint'].setup { capabilities = capabilities }
config['sumneko_lua'].setup {
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
        path = vim.split(package.path, ";"),
      },
      diagnostics = { globals = { "vim", "hs" } },
      workspace = {
        [vim.fn.expand "$VIMRUNTIME/lua"] = true,
        [vim.fn.stdpath "config" .. "/lua"] = true,
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = { enable = false },
    },
  },
}
config["terraformls"].setup({ capabilities = capabilities })
config["tflint"].setup({ capabilities = capabilities })
config["jdtls"].setup({ capabilities = capabilities })
