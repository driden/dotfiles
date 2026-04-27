---@class driden.UI
local M = {}

---@class FloatView
---@field win number window handle
---@field buf number buffer number
---@field row number buffer #row it starts
---@field col number buffer #column it starts
---@field height number buffer height
---@field width number buffer width

---@class FloatViewOpts
---@field height? integer
---@field width? integer
---@field row? integer
---@field col? integer
---@field title? string

--- Create a centered floating window
---@param opts FloatViewOpts
---@return FloatView
function M.create_floatview(opts)
  local buf = vim.api.nvim_create_buf(false, true)

  local width = opts.width or math.floor(vim.o.columns * 0.8)
  local height = opts.height or math.floor(vim.o.lines * 0.8)

  local center_col = math.floor((vim.o.columns - width) / 2)
  local center_row = math.floor((vim.o.lines - height) / 2)

  local col = opts.col or math.max(0, center_col)
  local row = opts.row or math.max(0, center_row)

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    style = "minimal",
    border = "rounded",
    width = width,
    height = height,
    row = row,
    col = col,
    title = opts.title or nil,
    title_pos = "center",
    focusable = true,
  })
  return { buf = buf, win = win, row = row, col = col, width = width, height = height }
end

--- Close a floating window and delete its buffer
---@param fv FloatView?
function M.close_floatview(fv)
  if not fv then return end
  if vim.api.nvim_win_is_valid(fv.win) then
    vim.api.nvim_win_close(fv.win, true)
  end
  if vim.api.nvim_buf_is_valid(fv.buf) then
    vim.api.nvim_buf_delete(fv.buf, { force = true })
  end
end

return M
