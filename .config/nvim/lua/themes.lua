local function set_colorscheme(scheme)
  vim.cmd("colorscheme " .. scheme)
end

local M = {}

M.theme = "PaperColor"
M.available_themes = {
  { name = "palenight", plugin = "drewtempelmeyer/palenight.vim" },
  { name = "onedark", plugin = "joshdick/onedark.vim" },
  { name = "gruvbox", plugin = "sainnhe/gruvbox-material" },
  { name = "PaperColor", plugin = "papercolor-theme" }
}

set_colorscheme(M.theme)

return M
