local M = {}
M.available_themes = {
  { id = 1, name = "palenight", plugin = "drewtempelmeyer/palenight.vim" },
  { id = 2, name = "onedark", plugin = "joshdick/onedark.vim" },
  { id = 3, name = "gruvbox-material", plugin = "sainnhe/gruvbox-material", bar = "gruvbox-material" },
  { id = 4, name = "jellybeans", plugin = "metalelf0/jellybeans-nvim" },
  { id = 5, name = "PaperColor", bar = "papercolor", plugin = "papercolor-theme" },
}

local function find_theme_by_name(name)
  for _, v in ipairs(M.available_themes) do
    if v.name == name then
      return v
    end
  end
end

local function set_colorscheme_by_name(name)
  local theme = find_theme_by_name(name)

  vim.cmd("colorscheme " .. theme.name)
  vim.cmd("let g:airline_theme='" .. theme.bar .. "'")
end

M.theme_name = "gruvbox-material"

function M.load_theme()
  set_colorscheme_by_name("gruvbox-material")

  -- Nicer symbols!
  local airline_symbols = {
    { var = "airline_left_alt_sep", symbol = "" },
    { var = "airline_left_sep", symbol = "" },
    { var = "airline_right_alt_sep", symbol = "" },
    { var = "airline_right_sep", symbol = "" },
    { var = "airline_symbols.branch", symbol = "" },
    { var = "airline_symbols.crypt", symbol = "🔒" },
    { var = "airline_symbols.linenr", symbol = "☰" },
    { var = "airline_symbols.maxlinenr", symbol = "" },
    { var = "airline_symbols.notexists", symbol = "Ɇ" },
    { var = "airline_symbols.paste", symbol = "ρ" },
    { var = "airline_symbols.readonly", symbol = "" },
    { var = "airline_symbols.spell", symbol = "Ꞩ" },
    { var = "airline_symbols.whitespace", symbol = "Ξ" },
  }

  for _, p in pairs(airline_symbols) do
    vim.g[p.var] = p.symbol
  end
end

vim.api.nvim_create_user_command("ChangeTheme", function(data)
  local args = data.args
  set_colorscheme_by_name(args)
end, {
  nargs = 1,
  complete = function()
    -- return completion candidates as a list-like table
    return { "foo", "bar", "baz" }
  end,
})

return M
