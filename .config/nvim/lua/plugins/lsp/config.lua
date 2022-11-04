-- Langs auto register themselved on the servers list, with their setup function
require("plugins.lsp.langs.sumneko")

local names = {}
local fs = {}
for _, v in ipairs(require("plugins.lsp.servers").servers) do
  local val = vim.tbl_values(v)
  table.insert(names, val[1])
  table.insert(fs, val[2])
end

require("mason-lspconfig").setup({
	ensure_installed = names
})

for _, setup in ipairs(fs) do
  setup()
end
