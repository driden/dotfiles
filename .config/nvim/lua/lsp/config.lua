-- Langs auto register themselved on the servers list, with their setup function
--
-- To add new servers
-- 1) create file langs/<lang>.lua
-- 2) write setup function and register it to the servers list
-- 3) require it here
-- TODO::
--  List all the files in the langs subfolder and just require everything automatically
require("mason").setup()
require("mason-lspconfig").setup()

-- require("lsp.langs.eslint")
require("lsp.langs.barium")
require("lsp.langs.bashls")
require("lsp.langs.sumneko")
require("lsp.langs.ts")
require("lsp.langs.go")
require("lsp.langs.python")
require("lsp.langs.terraform")

local fs = {}
local names = {}
for _, v in ipairs(require("lsp.servers").servers) do
  -- local val = vim.tbl_values(v)
  -- table.insert(names, val[1])
  -- table.insert(fs, val[2])
end

require("mason-lspconfig").setup({
  ensure_installed = names,
})

for _, setup in ipairs(fs) do
  setup()
end
