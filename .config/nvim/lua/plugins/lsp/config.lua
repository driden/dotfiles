-- Langs auto register themselved on the servers list, with their setup function
--
-- To add new servers
-- 1) create file langs/<lang>.lua
-- 2) write setup function and register it to the servers list
-- 3) require it here
-- TODO:: 
--  List all the files in the langs subfolder and just require everything automatically
require("plugins.lsp.langs.sumneko")
require("plugins.lsp.langs.ts")

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
