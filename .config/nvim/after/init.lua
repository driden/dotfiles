-- if vim.g.vscode then
--     return
-- else
-- print("not vs code")
-- end

vim.print("after")
require("snippets")
require("completion")
require("lsp")
require("file_tree").setup()
require("hop")
require("indent-guides")
require("telescope")
require("treesitter").setup()
require("comment")
require("transparent")
