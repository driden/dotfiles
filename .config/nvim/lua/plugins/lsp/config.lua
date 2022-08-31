
local lspconfig = require("lspconfig")
lspconfig['tsserver'].setup {}
lspconfig['bashls'].setup {}
lspconfig['eslint'].setup {}
lspconfig['sumneko_lua'].setup {
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


local capabilities = require("cmp_nvim_lsp")
  .update_capabilities(vim.lsp.protocol.make_client_capabilities())
require("lspconfig")["terraformls"].setup({ capabilities = capabilities })
require("lspconfig")["tflint"].setup({ capabilities = capabilities })
require("lspconfig")["jdtls"].setup({ capabilities = capabilities })
