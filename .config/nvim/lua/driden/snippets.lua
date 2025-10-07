local ls = require("luasnip")
ls.config.setup({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
})

for _, ft_path in ipairs(vim.api.nvim_get_runtime_file("lua/driden/snippets/*.lua", true)) do
  loadfile(ft_path)()
end
