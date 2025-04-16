return {
  "L3MON4D3/LuaSnip",
  build = (function()
    -- Build Step is needed for regex support in snippets.
    -- This step is not supported in many windows environments.
    -- Remove the below condition to re-enable on windows.
    if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
      return
    end
    return "make install_jsregexp"
  end)(),
  config = function()
    require("luasnip.loaders.from_vscode").lazy_load({ paths = { "~/.config/nvim/snippets" } })

    local ls = require("luasnip")

    ls.config.setup({
      history = true,
      updateevents = "TextChanged,TextChangedI",
      enable_autosnippets = true,
    })

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
    -- vim.keymap.set({ "i", "s" }, "<C-E>", function()
    --   if ls.choice_active() then
    --     ls.change_choice(1)
    --   end
    -- end, { silent = true })
  end,

  dependencies = {
    -- `friendly-snippets` contains a variety of premade snippets.
    --    See the README about individual language/framework/plugin snippets:
    --    https://github.com/rafamadriz/friendly-snippets
    {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    },
  },
}
