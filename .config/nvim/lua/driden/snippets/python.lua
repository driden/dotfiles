local ls = require("luasnip")
ls.config.setup({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

ls.cleanup()

local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_nodek
local fmt = require("luasnip.extras.fmt").fmt

local main = s(
  "__main",
  fmt(
    [[
#!/usr/bin/env python3

def main():
    {}

if __name__ == "main":
    main()
 ]],
    {
      i(0, "code"),
    }
  )
)
ls.add_snippets("python", { main })
