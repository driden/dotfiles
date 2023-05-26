local M = {}
M.theme = "kanagawa"
M.available_themes = {
  { name = "tokyonight-night", bar = "onedark",          plugin = "joshdick/onedark.vim",          opts = {} },
  { name = "palenight",        bar = "palenight",        plugin = "drewtempelmeyer/palenight.vim", opts = {} },
  { name = "onedark",          bar = "onedark",          plugin = "joshdick/onedark.vim",          opts = {} },
  { name = "kanagawa",         bar = "onedark",          plugin = "rebelot/kanagawa.nvim",         opts = {} },
  { name = "gruvbox-material", bar = "gruvbox_material", plugin = "sainnhe/gruvbox-material",      opts = {} },
  { name = "PaperColor",       bar = "papercolor",       plugin = "papercolor-theme",              opts = {} },
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
end

function M.set_theme()
  set_colorscheme(M.theme)
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
  M.set_colorscheme(args)
end, {
  nargs = 1,
  complete = function()
    return map(function(theme)
      return theme.name
    end, M.available_themes)
  end,
})

return M
