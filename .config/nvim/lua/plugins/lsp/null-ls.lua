-- If you are looking for java, then its at ../../ftplugin/

local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics

require("null-ls").setup {
  debug = false, -- :NullLsLog && :NullLsInfo
  sources = {
    formatting.eslint,
    formatting.shfmt,
    formatting.stylua,
    formatting.eslint,
    diagnostics.eslint,
  },
  on_attach = require('plugins.lsp.keymaps').on_attach
}
