local M = {}
M.available_themes = {
  { id = 1, name = "palenight", plugin = "drewtempelmeyer/palenight.vim" },
  { id = 2, name = "onedark", plugin = "joshdick/onedark.vim" },
  { id = 3, name = "gruvbox", plugin = "sainnhe/gruvbox-material" },
  { id = 4, name = "jellybeans", plugin = "metalelf0/jellybeans-nvim" },
  { id = 5, name = "PaperColor", bar = "papercolor", plugin = "papercolor-theme" }
}

local function find_theme_by_id(id)
  for _, v in ipairs(M.available_themes) do
    if (v.id == id) then
      return v
    end
  end
end

local function set_colorscheme_by_id(id)
  local theme = find_theme_by_id(id)

  vim.cmd("colorscheme " .. theme.name)
  vim.cmd("let g:airline_theme='" .. theme.bar .. "'")
end

M.theme_id = 5

function M.load_theme()
  set_colorscheme_by_id(5)
end

return M
