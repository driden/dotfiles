local M = {}

local util = require("git-perma.util")

---@class GitPermaLink.RangeInfo
---@field start_line number The starting line of the range
---@field end_line number The ending line of the range

---@class GitPermaLink.Repo
---@field host string ex github.com
---@field path string owner/repo

---@type table<string, fun(repo: GitPermaLink.Repo, sha: string, path: string, range: GitPermaLink.RangeInfo): string>
M.host_formatters = {
  ["github.com"] = function(repo, sha, path, range)
    local fragment
    if range.start_line == range.end_line then
      fragment = "#L" .. range.start_line
    else
      fragment = "#L" .. range.start_line .. "-L" .. range.end_line
    end
    return string.format("https://%s/%s/blob/%s/%s%s", repo.host, repo.path, sha, path, fragment)
  end,

  -- UNTESTED below
  -- ["gitlab.com"] = function(repo, sha, path, range)
  --   local fragment
  --   if range.start_line == range.end_line then
  --     fragment = "#L" .. range.start_line
  --   else
  --     fragment = "#L" .. range.start_line .. "-" .. range.end_line
  --   end
  --   return string.format("%s/-/blob/%s/%s%s", base_url, sha, path, fragment)
  -- end,
  --
  -- ["bitbucket.org"] = function(repo, sha, path, range)
  --   local fragment
  --   -- Bitbucket requires the filename in the fragment
  --   local file_name = vim.fn.fnamemodify(path, ":t")
  --   if range.start_line == range.end_line then
  --     fragment = string.format("#%s-%d", file_name, range.start_line)
  --   else
  --     fragment = string.format("#lines-%d:%d", range.start_line, range.end_line)
  --   end
  --   return string.format("%s/src/%s/%s%s", base_url, sha, path, fragment)
  -- end,

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
--- @return GitPermaLink.Repo
function M.parse_remote_url(remote)
  local host, path

  -- 1. Try SSH format: git@github.com:user/repo.git
  host, path = remote:match("^git@(.-):(.*)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return { host = host, path = path }
  end

  -- 2. Try HTTPS format: [https://github.com/user/repo.git](https://github.com/user/repo.git)
  host, path = remote:match("^https://(.-)/(.+)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return { host = host, path = path }
  end

  -- 3. Try SSH protocol format: ssh://git@github.com/user/repo
  host, path = remote:match("^ssh://git@(.-)/(.+)")
  if host then
    path = path:gsub("%.git$", "") -- remove trailing.git
    return { host = host, path = path }
  end

  util.notify_error("failed to parse host")
  return { host = "", path = "" }
end

---Attempts to generate url
---@param git_data GitPermaLink.Remote
---@param line_range GitPermaLink.RangeInfo
---@return string?,string?
function M.generate_url(git_data, line_range)
  local repo = M.parse_remote_url(git_data.remote_url)
  if not repo.host then
    util.notify_error("Could not find remote host ")
    return nil, "Failed to parse remote URL: " .. git_data.remote_url
  end

  local formatter = M.host_formatters[repo.host]
  if not formatter then
    util.notify_error("Could not find formatter for " .. repo.host)
    return nil, "Unsupported Git host: " .. repo.host .. ". Add a custom formatter via setup()."
  end

  return formatter(repo, git_data.sha, git_data.relative_path, line_range)
end

return M
