---@class driden.Util
local M = {}

--- Get 0-based start and end rows from visual selection, then exit visual mode
---@return integer start_row, integer end_row
function M.visual_range()
  local sr = vim.fn.line("v") - 1
  local er = vim.fn.line(".") - 1
  if sr > er then sr, er = er, sr end
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", false)
  return sr, er
end

--- Walk up from a treesitter node to find an ancestor of the given type
---@param node TSNode?
---@param type string
---@return TSNode?
function M.find_ancestor(node, type)
  while node and node:type() ~= type do
    node = node:parent()
  end
  return node
end

--- Escape Lua pattern magic characters in a string
---@param s string
---@return string
function M.escape_regex(s)
  return s:gsub("([%(%)%.%%%+%-%*%?%[%^%$])", "%%%1")
end

return M
