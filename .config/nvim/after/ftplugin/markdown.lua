vim.opt_local.conceallevel = 1

vim.keymap.set(
  { "v", "x" },
  "<localleader>e",
  ":ObsidianExtractNote<CR>",
  { silent = true, buffer = true, desc = "Extract to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>l",
  ":ObsidianLink<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>L",
  ":ObsidianLinkNew<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set("n", "<localleader>n", ":ObsidianNew<CR>", { silent = true, buffer = true, desc = "New note" })
vim.keymap.set(
  "n",
  "<localleader>N",
  ":ObsidianNewFromTemplate<CR>",
  { silent = true, buffer = true, desc = "New note from Template" }
)
vim.keymap.set("n", "<localleader>f", ":ObsidianQuickSwitch<CR>", { silent = true, buffer = true, desc = "Find Note" })
vim.keymap.set("n", "<localleader>T", ":ObsidianToday<CR>", { silent = true, buffer = true, desc = "Today's note" })
vim.keymap.set(
  "n",
  "<localleader>t",
  ":ObsidianTemplate<CR>",
  { silent = true, buffer = true, desc = "Insert Template" }
)
vim.keymap.set(
  "n",
  "<localleader>w",
  ":ObsidianWorkspace<CR>",
  { silent = true, buffer = true, desc = "Open workspace" }
)
