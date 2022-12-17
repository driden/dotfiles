require("trouble").setup({})

local nnoremap = require("driden.keymaps").nnoremap
--
-- Lua
nnoremap("<leader>xx", "<cmd>TroubleToggle<cr>")
nnoremap("gt", "<cmd>TroubleToggle workspace_diagnostics<cr>")
nnoremap("gq", "<cmd>TroubleToggle quickfix<cr>")
