local M = {}

--- A centralized notification function
---@param msg string the message
---@param level integer?
function M.notify(msg, level)
  -- vim.notify can take a title to make notifications clearer
  vim.notify(msg, level or vim.log.levels.INFO, { title = "Git-Perma" })
end

--- A dedicated error notification function
--- @param msg string the error message
function M.notify_error(msg)
  M.notify(msg, vim.log.levels.ERROR)
end

-- Fallback clipboard copy using system tools
---@return boolean success
function M.copy_to_clipboard_fallback(text)
  local cmd
  local clipboard_tools = {
    mac = { ["pbcopy"] = "pbcopy" },
    unix = {
      ["wl-copy"] = "wl-copy", -- Wayland
      ["xclip"] = { "-selection", "clipboard", "-in" }, -- X11
      ["xsel"] = { "xsel", "--clipboard", "--input" },
    },
    win32 = { ["clip.exe"] = "clip.exe" },
  }

  local os_type = vim.fn.has("mac") == 1 and "mac"
    or vim.fn.has("unix") == 1 and "unix"
    or vim.fn.has("win32") == 1 and "win32"

  if os_type == "unix" then
    for tool, cmd in pairs(clipboard_tools.unix) do
      if vim.fn.executable(tool) == 1 then
        cmd = cmd
        break
      end
    end
  else
    cmd = clipboard_tools[os_type]
  end

  if not cmd then
    return false
  end

  -- Try to write to clipboard
  local ok, result = pcall(vim.fn.system, cmd, text)
  if ok and vim.v.shell_error == 0 then
    return true
  end

  M.notify_error("Couldn't copy link for " .. os_type .. "with " .. cmd)
  return false
end

return M
