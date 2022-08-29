local M = {}
M.theme = "gruvbox-material"
M.available_themes = {
  { name = "palenight", bar = "palenight", plugin = "drewtempelmeyer/palenight.vim", opts = {} },
  { name = "onedark", bar = "onedark", plugin = "joshdick/onedark.vim", opts = {} },
  { name = "gruvbox-material", bar = "gruvbox_material", plugin = "sainnhe/gruvbox-material", opts = {} },
  { name = "jellybeans-nvim", bar = "jellybeans", plugin = "metalelf0/jellybeans-nvim", opts = {} },
  { name = "PaperColor", bar = "papercolor", plugin = "papercolor-theme", opts = {} },
}

local function find_theme(name)
  for _, v in ipairs(M.available_themes) do
    if v.name == name then
      return v
    end
  end
end

local function set_colorscheme(name)
  local theme = find_theme(name)
  vim.cmd("colorscheme " .. theme.name)
  vim.cmd("let g:airline_theme='" .. theme.bar .. "'")
  print(name)
end

function M.load_theme()
  set_colorscheme(M.theme)

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

function M.set_next_theme()
  local idx = -1
  for i, v in ipairs(M.available_themes) do
    if v.name == M.theme then
      idx = i
      break
    end
  end
  idx = 1 + (idx % #M.available_themes)
  M.theme = M.available_themes[idx].name
  set_colorscheme(M.theme)
end

local map = require("utils.collections").map
vim.api.nvim_create_user_command("ChangeTheme", function(data)
  local args = data.args
  set_colorscheme(args)
end, {
  nargs = 1,
  complete = function()
    return map(function(theme)
      return theme.name
    end, M.available_themes)
  end,
})

return M
