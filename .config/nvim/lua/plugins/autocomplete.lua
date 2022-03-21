-- COMPE
local M = {}

 local cmp = require("cmp")
 local lspkind = require('lspkind')

 function M.setup()
   cmp.setup({
     completion = {
       autocomplete = { },
     },
     snippet = {
       expand = function(args)
         vim.fn["vsnip#anonymous"](args.body)
       end,
     },
     documentation = { },
     sorting = {
       priority_weight = 2.,
       comparators = { },
     },
     mapping = {
       ['<C-y>'] = cmp.mapping.confirm{ select = true },
       -- Defaults from github
       ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
       ['<C-d>'] = cmp.mapping.scroll_docs(-4),
       ['<C-f>'] = cmp.mapping.scroll_docs(4),
       ['<C-Space>'] = cmp.mapping.complete(),
       ['<C-e>'] = cmp.mapping.close(),
       ['<CR>'] = cmp.mapping.confirm({
         behavior = cmp.ConfirmBehavior.Replace,
         select = true,
       })
     },
     sources = {
       { name = "nvim_lsp" },
       { name = "treesitter" },
       { name = "emoji" },
       { name = "orgmode" },
       { name = "buffer" }
     },
    formatting = {
      format = lspkind.cmp_format({
        mode = 'symbol', -- show only symbol annotations
        maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

        -- The function below will be called before any actual modifications from lspkind
        -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
        before = function (entry, vim_item)
          return vim_item
        end
      })
    }})
  end
return M
