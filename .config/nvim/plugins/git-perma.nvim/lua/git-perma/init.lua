local M = {}

local util = require("git-perma.util")

---@class GitPermaLink.config
---@field hosts string[]
M.config = {
  hosts = {},
}

--- @return boolean
local function is_git_dir()
  local buf_dir = vim.fn.expand("%:p:h")
  if buf_dir == "" or vim.fn.finddir(".git", buf_dir .. ";") == "" then
    return false -- Not in a git repo, don't register anything for this buffer
  end

  return true
end

function M.setup(opts)
  if not is_git_dir() then
    return
  end

  -- Define user command with range support
  vim.api.nvim_create_user_command("GitPermalink", function(args)
    local mode = args.range > 0 and "visual" or "normal"
    local action = args.args == "open" and "open" or "copy"
    local ok, err = pcall(require("git-perma.core").generate_link, mode, action)
    if not ok then
      require("git-perma.util").notify_error(err or "ERROR")
    end
  end, {
    desc = "Generate Git permalink (copy to clipboard or open in browser)",
    range = true,
    nargs = "?",
    complete = function()
      return { "open", "copy" }
    end,
  })

  -- Define the keymap for normal mode [15] - buffer local
  vim.keymap.set("n", "<leader>gl", function()
    -- pcall ensures any synchronous error is caught [18]
    local ok, err = pcall(require("git-perma.core").generate_link, "normal", "copy")
    if not ok then
      require("git-perma.util").notify_error(err or "ERROR")
    end
  end, {
    desc = "GitLink: Copy permalink for current line",
    buffer = 0, -- Buffer-local keymap
  })

  -- Define keymap to copy permalink (visual mode)
  vim.keymap.set("v", "<leader>gl", function()
    local start_line, end_line = util.get_visual_range()
    local ok, err = pcall(require("git-perma.core").generate_link_with_positions, start_line, end_line, "copy")
    if not ok then
      require("git-perma.util").notify_error(err or "ERROR")
    end
  end, {
    desc = "GitLink: Copy permalink for selection",
    buffer = 0, -- Buffer-local keymap
  })

  -- Define keymap to open in browser (normal mode)
  vim.keymap.set("n", "<leader>gL", function()
    local ok, err = pcall(require("git-perma.core").generate_link, "normal", "open")
    if not ok then
      require("git-perma.util").notify_error(err or "ERROR")
    end
  end, {
    desc = "GitLink: Open permalink in browser",
    buffer = 0, -- Buffer-local keymap
  })

  -- Define keymap to open in browser (visual mode)
  vim.keymap.set("v", "<leader>gL", function()
    local start_line, end_line = util.get_visual_range()

    local ok, err = pcall(require("git-perma.core").generate_link_with_positions, start_line, end_line, "open")
    if not ok then
      require("git-perma.util").notify_error(err or "ERROR")
    end
  end, {
    desc = "GitLink: Open permalink for selection in browser",
    buffer = 0, -- Buffer-local keymap
  })
end
return M
