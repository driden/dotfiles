require("luasnip.loaders.from_vscode").lazy_load({ paths = { "~/.config/nvim/snippets" } })

local ls = require("luasnip")

ls.config.setup({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

vim.keymap.set({ "i", "s" }, "<c-j>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<c-l>", function()
  if ls.choice_active() then
    ls.change_choice()
  end
end, { silent = true })

local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_nodek
local fmt = require("luasnip.extras.fmt").fmt

-- available in all languages.
ls.add_snippets("all", {
  s(
    "<?",
    fmt("{} ? {}: {}", {
      -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
      i(1, "cond"),
      i(2, "then"),
      i(3, "else"),
    })
  ),
})

ls.add_snippets("typescript", {
  s(
    "<ita",
    fmt(
      [[it("{}", async () => {{
      {}
      }});
      ]],
      {
        -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
        i(1, "description"),
        i(0, "code"),
      }
    )
  ),
})
vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/after/plugin/snippets.lua<CR>")
