vim.opt_local.conceallevel = 1

vim.keymap.set(
  { "v", "x" },
  "<localleader>e",
  ":Obsidian extract_note<CR>",
  { silent = true, buffer = true, desc = "Extract to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>l",
  ":Obsidian link<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>L",
  ":Obsidian link_new<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set("n", "<localleader>n", ":Obsidian new<CR>", { silent = true, buffer = true, desc = "New note" })
vim.keymap.set(
  "n",
  "<localleader>N",
  ":Obsidian new_from_template<CR>",
  { silent = true, buffer = true, desc = "New note from Template" }
)
vim.keymap.set(
  "n",
  "<localleader>f",
  ":Obsidian quick_switch<CR>",
  { silent = true, buffer = true, desc = "Find Note" }
)
vim.keymap.set("n", "<localleader>T", ":Obsidian today<CR>", { silent = true, buffer = true, desc = "Today's note" })
vim.keymap.set(
  "n",
  "<localleader>t",
  ":Obsidian template<CR>",
  { silent = true, buffer = true, desc = "Insert Template" }
)
vim.keymap.set(
  "n",
  "<localleader>w",
  ":Obsidian workspace<CR>",
  { silent = true, buffer = true, desc = "Open workspace" }
)
