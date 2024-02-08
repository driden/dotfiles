local M = {}

local commands = {
  { name = "Config",              action = ":edit $HOME/.config/nvim/init.lua" },
  { name = "ReloadConfig",        action = ":source ~/.config/nvim/init.lua" },
  -- stylua: ignore start
  { name = "RemoveEscapedQuotes", action = '%s#\\\\"#\"#g' },
  -- stylua: ignore end
  {
    name = "CopyPath",
    action = function(data)
      local file = vim.fn.expand("%:p")
      vim.fn.setreg("+", file)
      vim.notify("Copied file path to clipboard!", 2)
    end
  },
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
