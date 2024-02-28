require("telescope").setup({
  defaults = {
    file_ignore_patterns = { "vendor", "node_modules" },
    file_sorter = require("telescope.sorters").get_fzy_sorter,
    color_devicons = true,
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
  },
  pickers = {},
})

vim.api.nvim_set_keymap(
  "n",
  "<leader>bD",
  ":lua require('driden.telescope').del_buffers()<cr>",
  { noremap = true, silent = false }
)

vim.api.nvim_set_keymap(
  "n",
  '<leader>f"',
  ":lua require('telescope.builtin').registers()<cr>",
  { noremap = true, silent = false }
)

vim.api.nvim_set_keymap(
  "n",
  "<leader>fb",
  ":lua require('telescope.builtin').buffers()<cr>",
  { noremap = true, silent = false }
)

vim.api.nvim_set_keymap(
  "n",
  "<leader>fe",
  ":lua require('telescope.builtin').diagnostics()<cr>",
  { noremap = true, silent = false }
)

-- Telescope
vim.keymap.set("n", "<leader>ff", function()
  require("telescope.builtin").find_files({ hidden = true })
end, { noremap = true, silent = false })
vim.keymap.set("n", "<leader>ft", function()
  require("telescope.builtin").live_grep()
end, { noremap = true, silent = false })
vim.keymap.set("n", "<leader>fk", function()
  require("telescope.builtin").keymaps()
end, { noremap = true, silent = false })
vim.keymap.set("n", "<leader>fb", function()
  require("telescope.builtin").buffers()
end, { noremap = true, silent = false })

vim.keymap.set("n", "<leader>fh", function()
  require("telescope.builtin").help_tags()
end, { noremap = true, silent = false })
