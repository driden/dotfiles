local M = {}

local commands = {
  { name = "EditConfig", action = ":edit $HOME/.config/nvim/init.lua" },
  { name = "ReloadConfig", action = ":source ~/.config/nvim/init.lua" },
  { name = "RemoveEscapedQuotes", action = ':%s/\\"/"/g' },
}

local function create_command(name, action)
  vim.api.nvim_create_user_command(name, action, {})
end

function M.load_commands()
  for _, value in ipairs(commands) do
    create_command(value.name, value.action)
  end
end

return M
