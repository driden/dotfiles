local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_nodek
local fmt = require("luasnip.extras.fmt").fmt

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
