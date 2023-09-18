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
