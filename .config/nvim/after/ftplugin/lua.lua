vim.keymap.set("n", "<leader><S-x>", ":luafile %<CR>", { desc = "execute: file", noremap = true, silent = true })

-- help :{range}lua
vim.keymap.set(
  { "x", "v" },
  "<leader>x",
  "<cmd>'<,'>lua<CR>",
  { desc = "execute: selected", noremap = true, silent = true, buffer = true }
)
