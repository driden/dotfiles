local cmp = require("cmp")
local ls = require("luasnip")

local has_words_before = function()
	unpack = unpack or table.unpack
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
cmp.setup({
	completion = {
		autocomplete = true,
	},
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
		end,
	},
	mapping = {
		["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
		["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		["<Enter>"] = cmp.mapping(cmp.mapping.confirm(), { "i", "c" }),
		["<TAB>"] = cmp.mapping(function(fallback)
			if ls.expand_or_jumpable() then
				ls.expand_or_jump()
			elseif cmp.visible() or has_words_before() then
				cmp.mapping.confirm({
					behavior = cmp.ConfirmBehavior.Replace,
					select = true,
				})
			else
				fallback()
			end
		end, { "i", "c" }),
		["<C-y>"] = cmp.config.disable,
		["<C-g>"] = cmp.mapping.abort(),
	},
	sources = cmp.config.sources({
		-- { name = "omni" },
		-- { name = "nvim_lua" },
		{ name = "luasnip" },
		{ name = "nvim_lsp" },
		{ name = "nvim_lsp_signature_help" },
		{ name = "path" },
		{ name = "buffer", keyword_length = 4, max_item_count = 5 },
	}),
	formatting = {
		format = require("lspkind").cmp_format({
			maxwidth = 50,
			mode = "symbol",
			preset = "codicons",
			symbol_map = {
				Text = "",
				Method = "",
				Function = "",
				Constructor = "",
				Field = "ﰠ",
				Variable = "",
				Class = "ﴯ",
				Interface = "",
				Module = "",
				Property = "ﰠ",
				Unit = "塞",
				Value = "",
				Enum = "",
				Keyword = "",
				Snippet = "",
				Color = "",
				File = "",
				Reference = "",
				Folder = "",
				EnumMember = "",
				Constant = "",
				Struct = "פּ",
				Event = "",
				Operator = "",
				TypeParameter = "",
			},
		}),
	},
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ "/", "?" }, {
	mapping = cmp.mapping.preset.cmdline(),
	sources = {
		{ name = "buffer" },
	},
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({
		{ name = "path" },
	}, {
		{ name = "cmdline" },
	}),
})

vim.api.nvim_create_autocmd({ "TextChangedI", "TextChangedP" }, {
	callback = function()
		local line = vim.api.nvim_get_current_line()
		local cursor = vim.api.nvim_win_get_cursor(0)[2]

		local current = string.sub(line, cursor, cursor + 1)
		if current == "." or current == "," or current == " " then
			require("cmp").close()
		end

		local before_line = string.sub(line, 1, cursor + 1)
		local after_line = string.sub(line, cursor + 1, -1)
		if not string.match(before_line, "^%s+$") then
			if after_line == "" or string.match(before_line, " $") or string.match(before_line, "%.$") then
				require("cmp").complete()
			end
		end
	end,
	pattern = "*",
})
