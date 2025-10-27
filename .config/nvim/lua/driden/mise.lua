---
-- Gets the 'mise' bin path for a tool using Neovim's 'vim.fn.system' (sync).
--
-- This is the recommended method for use inside a Neovim config.
--
-- @param tool (string) # The tool name, e.g., "java"
-- # @return (string | nil) #The bin path, or nil on error
local function get_mise_bin_path_sync(tool)
  -- 1. Construct the command
  local command = "mise bin-paths " .. tool

  -- 2. Run the command synchronously
  -- vim.fn.system() returns the command's stdout
  local output = vim.fn.system(command)

  -- 3. Check for errors
  -- vim.v.shell_error holds the exit code of the last shell command
  if vim.v.shell_error ~= 0 then
    vim.notify("Error running command: " .. command .. " (Code: " .. vim.v.shell_error .. ")", vim.log.levels.ERROR)
    return nil
  end

  -- 4. Trim the trailing newline from the output
  local bin_path = output:gsub("[\n\r]$", "")

  if bin_path == "" then
    vim.notify("mise returned an empty path for " .. tool, vim.log.levels.WARN)
    return nil
  end

  return bin_path
end

return { get_mise_bin_path_sync = get_mise_bin_path_sync }
