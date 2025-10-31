vim.api.nvim_buf_create_user_command(0, "Jq", "%!jq", {})
vim.api.nvim_buf_create_user_command(0, "JqSortKeys", "%!jq --sort-keys", {})

if vim.version().minor < 10 then
  vim.notify("jq_query.nvim requires Neovim 0.10 or newer.", vim.log.levels.ERROR)
  return
end

if vim.fn.executable("jq") == 0 then
  vim.notify("jq executable not found in PATH.", vim.log.levels.ERROR)
  return
end

---@class FloatView
---@field win number window handle
---@field buf number buffer number

---@class win_config
---@field height integer | nil
---@field width integer | nil
---@field row integer | nil
---@field col integer | nil
---@field title string | nil

---@param opts win_config
---@return FloatView floatView
local function create_floatview(opts)
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    style = "minimal",
    border = "rounded",
    width = opts.width or math.floor(vim.o.columns * 0.8),
    height = opts.height or math.floor(vim.o.lines * 0.8),
    row = opts.row or math.floor(vim.o.lines * 0.1),
    col = opts.col or math.floor(vim.o.columns * 0.1),
    title = opts.title or nil,
    title_pos = "center",
    focusable = true,
  })
  return { buf = buf, win = win }
end

---Closes window and deletes buffer
---@param fv FloatView view
local function close_floatview(fv)
  if not fv then
    return
  end
  if vim.api.nvim_win_is_valid(fv.win) then
    vim.api.nvim_win_close(fv.win, true)
  end
  if vim.api.nvim_buf_is_valid(fv.buf) then
    vim.api.nvim_buf_delete(fv.buf, { force = true })
  end
end

---@class State
---@field source_buf number the source buffer
---@field input FloatView | nil the input view & buffer
---@field results FloatView | nil the results view & buffer
---@field prompt string the prompt used in the input buffer
---@field last_query string the jq query
---@field timer uv_timer_t | nil timer used to schedule jq calls

---@type State
local state = {
  input = nil,
  results = nil,
  prompt = "jq > ",
  source_buf = vim.api.nvim_get_current_buf(),
  last_query = "",
  timer = nil,
}

---Closes every window
local function shut_down()
  close_floatview(state.input)
  close_floatview(state.results)
  state.input = nil
  state.results = nil
  if state.timer then
    state.timer:close()
    state.timer = nil
  end
end

-- ---Runs jq and updates output buffer with results
-- ---@param query string
local function run_jq()
  local json_lines = vim.api.nvim_buf_get_lines(state.source_buf, 0, -1, false)
  local json_input = table.concat(json_lines, "\n")

  vim.system(
    { "jq", state.last_query },
    {
      stdin = json_input,
      text = true,
    },
    vim.schedule_wrap(function(result)
      if not state.results or not vim.api.nvim_buf_is_valid(state.results.buf) then
        return -- User closed the playground, do nothing
      end

      if result.code ~= 0 then
        local err_msg = vim.trim(result.stderr)
        local err_lines = vim.split("Error: " .. err_msg, "\n", { plain = true })
        vim.api.nvim_buf_set_lines(state.results.buf, 0, -1, false, err_lines)
      else
        local output = vim.split(vim.trim(result.stdout), "\n")
        vim.api.nvim_buf_set_lines(state.results.buf, 0, -1, false, output)
      end
    end)
  )
end

---Escapes regex magic characters in a string
---@param s string
---@return string
local function escape_regex(s)
  return s:gsub("([%(%)%.%%%+%-%*%?%[%^%$])", "%%%1")
end

---Gets the query written in the input buffer
---@return string The query without input prefix
local function parse_query()
  local escaped_prompt = escape_regex(state.prompt)
  -- Match the exact prompt, followed by zero or more spaces
  local pattern = string.format("^(%s) *", escaped_prompt)
  local query = vim.api.nvim_get_current_line():gsub(pattern, "")
  return query
end

---Calls jq with the input query in case it's required
local function schedule_update()
  local query = parse_query()

  if query == state.last_query then
    return
  end

  if state.last_query == "" then
    vim.api.nvim_buf_set_lines(state.results.buf, 0, -1, false, { "<empty query>" })
    return
  end

  state.last_query = query
  run_jq()
end

---Inits the windows and buffers, creates the timer and sets up the keybinds
local function init()
  -- prevent re-entry
  if state.input then
    return
  end

  state.input = create_floatview({
    row = 2,
    height = 1,
    title = "Jq Playground",
  })

  state.results = create_floatview({ row = 5, title = "Query Results" })

  vim.api.nvim_set_option_value("buftype", "prompt", { buf = state.input.buf })
  vim.fn.prompt_setprompt(state.input.buf, state.prompt)
  vim.api.nvim_set_option_value("filetype", "json", { buf = state.results.buf })

  vim.api.nvim_create_autocmd("WinLeave", {
    buffer = state.input.buf,
    callback = function()
      shut_down()
    end,
  })

  vim.keymap.set("n", "q", function()
    shut_down()
  end, { buffer = state.input.buf, nowait = true })

  vim.api.nvim_set_current_win(state.input.win)
  vim.cmd("startinsert")

  -- Callback on input change (live update)
  local timer = vim.uv.new_timer()
  vim.api.nvim_create_autocmd("TextChangedI", {
    buffer = state.input.buf,
    callback = function()
      timer:stop()
      timer:start(200, 0, vim.schedule_wrap(schedule_update))
    end,
  })

  -- Set up clipboard copy (<localleader>c)
  vim.keymap.set({ "n", "i" }, "<Enter>", function()
    local query = parse_query()
    vim.fn.setreg("+", query)
    vim.notify("Copied query!")
  end, { buffer = state.input.buf })
end

vim.keymap.set("n", "<localleader>q", function()
  local ok, err = pcall(init)
  if not ok then
    vim.notify("jq_query.nvim error: " .. err, vim.log.levels.ERROR)
  end
end, { buffer = 0 })
