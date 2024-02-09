require("harpoon").setup()

local nnoremap = require("driden.keymaps").nnoremap

nnoremap("<localLeader>ha", ":lua require(\"harpoon.mark\").add_file()<CR>")
nnoremap("<localLeader>h", ":lua require(\"harpoon.ui\").toggle_quick_menu()<CR>")
nnoremap("<localLeader>hp", ":lua require(\"harpoon.ui\").nav_prev()<CR>")
nnoremap("<localLeader>hn", ":lua require(\"harpoon.ui\").nav_next()<CR>")
nnoremap("<localLeader>ht", ":lua require(\"harpoon.term\").gotoTerminal(1)<CR>")

nnoremap("<C-1>", ":lua require(\"harpoon.ui\").nav_file(1)<CR>")
nnoremap("<C-2>", ":lua require(\"harpoon.ui\").nav_file(2)<CR>")
nnoremap("<C-3>", ":lua require(\"harpoon.ui\").nav_file(3)<CR>")

require("telescope").load_extension('harpoon')
