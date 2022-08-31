local servers = require("plugins.lsp.servers")
require("nvim-lsp-installer").setup {
  ensure_installed = servers.names
}
