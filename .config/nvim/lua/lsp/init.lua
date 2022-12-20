-- IMPORTANT: make sure to setup neodev BEFORE lspconfig
require("neodev").setup({
  -- add any options here, or leave empty to use the default settings
})

require("lsp.config")
require("lsp.null-ls")
require("lsp.test")
require("lsp.diagnostics")
