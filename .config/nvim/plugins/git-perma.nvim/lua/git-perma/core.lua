local M = {}
local git = require("git-perma.git")
local util = require("git-perma.util")

-- Internal function that performs the actual link generation
---@param file_path string  absolute path to current file
---@param line_range GitPermaLink.RangeInfo
---@param action 'copy'|'open'
local function do_generate_link(file_path, line_range, action)
  action = action or "copy"

  -- Call the asynchronous Git data pipeline (Section III)
  util.notify("Generating Git permalink...")
  git.get_git_data(file_path, function(data, err)
    if err then
      return util.notify_error("Git Error: " .. err)
    end

    -- If Git data is retrieved, call the URL generator (Section IV)
    local hosts = require("git-perma.hosts")

    -- Use pcall to safely generate the URL
    local ok, url_or_err = pcall(hosts.generate_url, data, line_range)

    if not ok then
      return util.notify_error("URL Generation Error: " .. url_or_err)
    elseif type(url_or_err) ~= "string" then
      -- This handles cases where generate_url returns (nil, "error message")
      vim.print(type(url_or_err))
      vim.print(url_or_err)
      return util.notify_error(url_or_err or "there was an erro with generate_url")
    end

    local url = url_or_err

    -- Copy to clipboard or open in browser (Section V)
    if action == "open" then
      -- Open URL in default browser
      local open_ok, open_err = pcall(vim.ui.open, url)
      if not open_ok then
        -- Fallback: copy to clipboard and notify user
        vim.fn.setreg("+", url)
        util.notify("Could not open browser. URL copied to clipboard instead.")
      else
        util.notify("Opening Git permalink in browser...")
      end
      if open_err then
        util.notify("Error copying ")
      end
    else
      -- Copy to clipboard with fallback
      local copy_ok = pcall(vim.fn.setreg, "+", url)
      if copy_ok then
        util.notify("Git permalink copied to clipboard.")
      else
        -- Try system clipboard tools as fallback
        local sys_ok = util.copy_to_clipboard_fallback(url)
        if sys_ok then
          util.notify("Git permalink copied to clipboard.")
        else
          util.notify_error("Failed to copy to clipboard. URL: " .. url)
        end
      end
    end
  end)
end

---This function is called from normal mode keymaps and commands
---@param mode 'normal'|'visual'
---@param action 'copy'|'open'
function M.generate_link(mode, action)
  -- 1. Get the current file path
  local file_path = vim.fn.expand("%:p")
  if file_path == "" or file_path == nil then
    return util.notify_error("No file buffer found.")
  end

  local line_range

  -- 2. Get the line range based on the mode
  if mode == "visual" then
    -- For visual mode, use marks (called from command mode after visual exit)
    local s_pos = vim.fn.getpos("'<")
    local e_pos = vim.fn.getpos("'>")

    -- Handle selecting upwards (start line > end line) [25]
    line_range = {
      start_line = math.min(s_pos[2], e_pos[2]),
      end_line = math.max(s_pos[2], e_pos[2]),
    }
  else -- 'normal' mode
    -- Get the current cursor position [25]
    local pos = vim.fn.getcurpos(0)
    line_range = {
      start_line = pos[2],
      end_line = pos[2],
    }
  end

  do_generate_link(file_path, line_range, action)
end

-- This function is called from visual mode keymaps with pre-captured positions
-- @param start_pos: position table from getpos("'<")
-- @param end_pos: position table from getpos("'>")
-- @param action: 'copy' or 'open'
function M.generate_link_with_positions(start_pos, end_pos, action)
  -- 1. Get the current file path
  local file_path = vim.fn.expand("%:p")
  if file_path == "" or file_path == nil then
    return util.notify_error("No file buffer found.")
  end

  -- 2. Build line range from captured positions
  -- start_pos[2] and end_pos[2] are line numbers
  local line_range = {
    start_line = math.min(start_pos[2], end_pos[2]),
    end_line = math.max(start_pos[2], end_pos[2]),
  }

  do_generate_link(file_path, line_range, action)
end

return M
