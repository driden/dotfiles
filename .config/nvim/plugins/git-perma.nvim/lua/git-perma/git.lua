local M = {}

-- Helper to run a single git command asynchronously using vim.uv
---@param cwd string working directory for the command
---@param args string[] command arguments (e.g., {"rev-parse", "HEAD"})
---@param callback fun(stdout: string?, err:string?) text to write to each file descriptor
local function run_git_cmd(cwd, args, callback)
  -- Create pipes for capturing stdout and stderr
  local stdout = vim.uv.new_pipe(false)
  local stderr = vim.uv.new_pipe(false)
  local io = { stdout = {}, stderr = {} }

  -- Spawn the git process
  local handle, _ = vim.uv.spawn("git", {
    args = vim.list_extend({ "-C", cwd }, args),
    stdio = { nil, stdout, stderr },
  }, function(code, _)
    -- Process exit callback - schedule to main thread for API safety
    vim.schedule(function()
      -- Close pipes
      stdout:close()
      stderr:close()

      if code ~= 0 then
        local err_msg = table.concat(io.stderr, "")
        callback(nil, string.format("Git failed (exit %d): %s", code, err_msg))
      else
        callback(table.concat(io.stdout, ""), nil)
      end
    end)
  end)

  if not handle then
    callback(nil, "Failed to spawn git process. Is git installed?")
    return
  end

  vim.uv.read_start(stdout, function(err, data)
    if err then
      -- Error reading stdout
    elseif data then
      table.insert(io.stdout, data)
    end
  end)

  vim.uv.read_start(stderr, function(_, data)
    if data then
      table.insert(io.stderr, data)
    end
  end)
end

---Select the best remote from a list of available remotes
---Prefers "origin" if it exists, otherwise uses the first available
---@param remotes_output string output from "git remote" command
---@return string? selected selected remote name
local function select_remote(remotes_output)
  local remotes = {}
  for remote in remotes_output:gmatch("%S+") do
    table.insert(remotes, remote)
  end

  if #remotes == 0 then
    return nil
  end

  -- Prefer "origin" if it exists
  for _, remote in ipairs(remotes) do
    if remote == "origin" then
      return remote
    end
  end

  -- Otherwise use the first available remote
  return remotes[1]
end

---@class GitPermaLink.Remote
---@field root string
---@field sha string
---@field remote_url string
---@field relative_path string
---@field remote_name string

---This function is non-blocking. It takes a file path and a callback.
---The callback will be invoked with (data, err)
---@param file_path string absolute path to the current file
---@param callback fun(data: GitPermaLink.Remote?, err:string?)
function M.get_git_data(file_path, callback)
  local cwd = vim.fn.fnamemodify(file_path, ":h")

  -- First, get list of available remotes
  run_git_cmd(cwd, { "remote" }, function(remotes_output, err)
    if err then
      callback(nil, "Failed to get git remotes: " .. err)
      return
    end

    -- Select which remote to use
    local selected_remote = select_remote(remotes_output)
    if not selected_remote then
      callback(nil, "No git remotes found in repository")
      return
    end

    vim.notify("Using remote: " .. selected_remote, vim.log.levels.INFO, { title = "Git-Perma" })

    local results = {}

    -- Define the git commands we need to run sequentially
    local commands = {
      { args = { "rev-parse", "--show-toplevel" }, name = "root" }, -- 1. Repository root
      { args = { "rev-parse", "HEAD" }, name = "sha" }, -- 2. Current commit SHA
      { args = { "remote", "get-url", selected_remote }, name = "remote_url" }, -- 3. Remote URL (dynamically selected)
      { args = { "ls-files", "--full-name", file_path }, name = "relative_path" }, -- 4. Relative file path
    }

    -- Recursively run commands in sequence
    local function run_next(idx)
      if idx > #commands then
        -- All commands completed successfully
        callback({
          root = vim.trim(results[1]),
          sha = vim.trim(results[2]),
          remote_url = vim.trim(results[3]),
          relative_path = vim.trim(results[4]),
          remote_name = selected_remote, -- Include which remote was used
        }, nil)
        return
      end

      local cmd = commands[idx]
      run_git_cmd(cwd, cmd.args, function(stdout, _)
        if err then
          callback(nil, err)
          return
        end

        results[idx] = stdout
        run_next(idx + 1)
      end)
    end

    -- Start the command chain
    run_next(1)
  end)
end

return M
