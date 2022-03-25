local M = {}

function M.setup()
	-- all these are default opts
	require("nvim-tree").setup({
		auto_close = false,
		auto_reload_on_write = true,
		disable_netrw = false,
		hide_root_folder = false,
		hijack_cursor = true,
		hijack_netrw = true,
		hijack_unnamed_buffer_when_opening = false,
		ignore_buffer_on_setup = false,
		open_on_setup = false,
		open_on_tab = false,
		sort_by = "name",
		update_cwd = true,
		view = {
			width = 20,
			height = 30,
			side = "left",
			preserve_window_proportions = true,
			number = false,
			relativenumber = false,
			signcolumn = "yes",
			mappings = {
				custom_only = false,
				list = {
					{ key = { "<CR>", "o", "<2-LeftMouse>" }, action = "edit" },
					{ key = "<C-e>", action = "edit_in_place" },
					{ key = { "O" }, action = "edit_no_picker" },
					{ key = { "<2-RightMouse>", "<C-]>" }, action = "cd" },
					{ key = "<C-v>", action = "vsplit" },
					{ key = "<C-x>", action = "split" },
					{ key = "<C-t>", action = "tabnew" },
					{ key = "<", action = "prev_sibling" },
					{ key = ">", action = "next_sibling" },
					{ key = "P", action = "parent_node" },
					{ key = "<BS>", action = "close_node" },
					{ key = "<Tab>", action = "preview" },
					{ key = "K", action = "first_sibling" },
					{ key = "J", action = "last_sibling" },
					{ key = "I", action = "toggle_git_ignored" },
					{ key = "H", action = "toggle_dotfiles" },
					{ key = "R", action = "refresh" },
					{ key = "a", action = "create" },
					{ key = "d", action = "remove" },
					{ key = "D", action = "trash" },
					{ key = "r", action = "rename" },
					{ key = "<C-r>", action = "full_rename" },
					{ key = "x", action = "cut" },
					{ key = "c", action = "copy" },
					{ key = "p", action = "paste" },
					{ key = "y", action = "copy_name" },
					{ key = "Y", action = "copy_path" },
					{ key = "gy", action = "copy_absolute_path" },
					{ key = "[c", action = "prev_git_item" },
					{ key = "]c", action = "next_git_item" },
					{ key = "-", action = "dir_up" },
					{ key = "s", action = "system_open" },
					{ key = "q", action = "close" },
					{ key = "g?", action = "toggle_help" },
					{ key = "W", action = "collapse_all" },
					{ key = "S", action = "search_node" },
					{ key = "<C-k>", action = "toggle_file_info" },
					{ key = ".", action = "run_file_command" },
				},
			},
		},
		hijack_directories = {
			enable = true,
			auto_open = true,
		},
		update_focused_file = {
			enable = true,
			update_cwd = true,
			ignore_list = {},
		},
		ignore_ft_on_setup = {},
		system_open = {
			cmd = nil,
			args = {},
		},
		diagnostics = {
			enable = true,
			show_on_dirs = true,
			icons = {
				hint = "",
				info = "",
				warning = "",
				error = "",
			},
		},
		filters = {
			dotfiles = false,
			custom = {},
			exclude = {},
		},
		git = {
			enable = true,
			ignore = true,
			timeout = 400,
		},
		actions = {
			change_dir = {
				enable = true,
				global = false,
			},
			open_file = {
				quit_on_open = false,
				resize_window = false,
				window_picker = {
					enable = true,
					chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
					exclude = {
						filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
						buftype = { "nofile", "terminal", "help" },
					},
				},
			},
		},
		trash = {
			cmd = "trash",
			require_confirm = true,
		},
		log = {
			enable = false,
			truncate = false,
			types = {
				all = false,
				config = false,
				git = false,
			},
		},
	})

	vim.g.nvim_tree_indent_markers = 1
	--nnoremap <C-n> :NvimTreeToggle<CR>
	--nnoremap <leader>r :NvimTreeRefresh<CR>
	--nnoremap <leader>n :NvimTreeFindFile<CR>
end

return M