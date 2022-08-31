local cmp = require("cmp")
cmp.setup({
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
		end,
	},
	mapping = {
		["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		["<TAB>"] = cmp.mapping(
			cmp.mapping.confirm({
				behavior = cmp.ConfirmBehavior.Insert,
				select = true,
			}),
			{ "i", "c" }
		),
		["<C-y>"] = cmp.config.disable,
		["<C-g>"] = cmp.mapping.abort(),
	},
	sources = cmp.config.sources({
		{ name = "omni", dup = 0 },
		{ name = "nvim_lua", dup = 0 },
		{ name = "nvim_lsp", dup = 0 },
		{ name = "nvim_lsp_signature_help", dup = 0 },
		{ name = "path", dup = 0 },
		{ name = "buffer", keyword_length = 4, max_item_count = 5, dup = 0 },
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

cmp.setup.cmdline(":", {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({
		{ name = "path", dup = 0 },
	}, {
		{ name = "cmdline", dup = 0 },
	}),
})
