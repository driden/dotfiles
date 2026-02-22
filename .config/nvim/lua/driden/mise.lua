--- Gets the 'mise' bin path for a tool using libuv (via vim.system).
---
--- This is the recommended method for use inside a Neovim config.
---
---@param tool string The tool name, e.g., "java"
---@return string|nil bin_path The bin path, or nil on error
local function get_mise_bin_path_sync(tool)
  -- Use vim.system() which uses libuv under the hood
  local result = vim.system({ "mise", "bin-paths", tool }, { text = true }):wait()

  -- Check for errors
  if result.code ~= 0 then
    vim.notify(string.format("Error running mise bin-paths %s (Code: %d)", tool, result.code), vim.log.levels.ERROR)
    return nil
  end

  -- Trim the trailing newline from stdout
  local bin_path = result.stdout:gsub("[\n\r]+$", "")

  if bin_path == "" then
    vim.notify("mise returned an empty path for " .. tool, vim.log.levels.WARN)
    return nil
  end

  return bin_path
end

--- Gets the 'mise' installation path for a tool using libuv (via vim.system).
---
--- This is useful for setting JAVA_HOME or similar environment variables.
---
---@param tool string The tool name with optional version, e.g., "java@corretto-21"
---@return string|nil install_path The installation path, or nil on error
local function get_mise_install_path_sync(tool)
  -- Use vim.system() which uses libuv under the hood
  local result = vim.system({ "mise", "where", tool }, { text = true }):wait()

  -- Check for errors
  if result.code ~= 0 then
    -- vim.notify(
    --   string.format("Error running mise where %s (Code: %d)", tool, result.code),
    --   vim.log.levels.ERROR
    -- )
    return nil
  end

  -- Trim the trailing newline from stdout
  local install_path = result.stdout:gsub("[\n\r]+$", "")

  if install_path == "" then
    vim.notify("mise returned an empty path for " .. tool, vim.log.levels.WARN)
    return nil
  end

  return install_path
end

return {
  get_mise_bin_path_sync = get_mise_bin_path_sync,
  get_mise_install_path_sync = get_mise_install_path_sync,
}
