local M = {}

---@class HostFormatter
---@field start_line number The starting line of the range
---@field end_line number The ending line of the range

---@type table<string, fun(base_url: string, sha: string, path: string, range: HostFormatter): string>
M.host_formatters = {
  ["github.com"] = function(base_url, sha, path, range)
    local fragment
    if range.start_line == range.end_line then
      fragment = "#L" .. range.start_line
    else
      fragment = "#L" .. range.start_line .. "-L" .. range.end_line
    end
    return string.format("%s/blob/%s/%s%s", base_url, sha, path, fragment)
  end,

  ["gitlab.com"] = function(base_url, sha, path, range)
    local fragment
    if range.start_line == range.end_line then
      fragment = "#L" .. range.start_line
    else
      fragment = "#L" .. range.start_line .. "-" .. range.end_line
    end
    return string.format("%s/-/blob/%s/%s%s", base_url, sha, path, fragment)
  end,

  ["bitbucket.org"] = function(base_url, sha, path, range)
    local fragment
    -- Bitbucket requires the filename in the fragment
    local file_name = vim.fn.fnamemodify(path, ":t")
    if range.start_line == range.end_line then
      fragment = string.format("#%s-%d", file_name, range.start_line)
    else
      fragment = string.format("#lines-%d:%d", range.start_line, range.end_line)
    end
    return string.format("%s/src/%s/%s%s", base_url, sha, path, fragment)
  end,

  -- Example: Gitea support (can be added by users via setup())
  -- ["gitea.example.com"] = function(base_url, sha, path, range)
  --   local fragment = range.start_line == range.end_line
  --     and "#L" .. range.start_line
  --     or "#L" .. range.start_line .. "-L" .. range.end_line
  --   return string.format("%s/src/commit/%s/%s%s", base_url, sha, path, fragment)
  -- end
}

--- Parses a git remote URL into its components
--- @param remote string The remote url
--- @return string?, string?, string?
function M.parse_remote_url(remote)
  local host, path

  -- 1. Try SSH format: git@github.com:user/repo.git
  host, path = remote:match("^git@(.-):(.*)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return "https://" .. host, host, path
  end

  -- 2. Try HTTPS format: [https://github.com/user/repo.git](https://github.com/user/repo.git)
  host, path = remote:match("^https://(.-)/(.+)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return "https://" .. host, host, path
  end

  -- 3. Try SSH protocol format: ssh://git@github.com/user/repo
  host, path = remote:match("^ssh://git@(.-)/(.+)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return "https://" .. host, host, path
  end

  vim.print("gitperma: failed to parse host")

  return nil, nil, nil
end

return M
