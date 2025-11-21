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
---@field row number buffer #row it starts
---@field col number buffer #column it starts
---@field height number buffer height
---@field width number buffer height

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

  local width = opts.width or math.floor(vim.o.columns * 0.8)
  local height = opts.height or math.floor(vim.o.lines * 0.8)

  -- 2. Calculate Centering (Total - Size) / 2
  local center_col = math.floor((vim.o.columns - width) / 2)
  local center_row = math.floor((vim.o.lines - height) / 2)

  -- 3. Use provided opts or fallback to calculated center
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

---Scrolls the results window
---@param keys string The vim normal mode keys to execute (e.g. "<C-d>")
local function scroll_results(keys)
  if state.results and vim.api.nvim_win_is_valid(state.results.win) then
    vim.api.nvim_win_call(state.results.win, function()
      -- Convert "<C-d>" into the actual Control+D byte code
      local term_codes = vim.api.nvim_replace_termcodes(keys, true, false, true)
      vim.cmd("normal! " .. term_codes)
    end)
  end
end

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

  if query == "" then
    vim.api.nvim_buf_set_lines(state.results.buf, 0, -1, false, { "<empty query>" })
    return
  end

  state.last_query = query
  run_jq()
end

local function init()
  -- prevent re-entry
  if state.input then
    return
  end

  state.results = create_floatview({
    title = "Query Results",
    width = math.floor(vim.o.columns * 0.9),
    height = math.floor(vim.o.lines * 0.9),
  })

  state.input = create_floatview({
    row = state.results.row - 3, -- we need 3 rows (1 for height + 2 for borders)
    col = state.results.col,
    width = state.results.width, -- Match the width of the results
    height = 1,
    title = "Jq Playground",
  })

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

  local timer = vim.uv.new_timer()
  vim.api.nvim_create_autocmd("TextChangedI", {
    buffer = state.input.buf,
    callback = function()
      timer:stop()
      timer:start(200, 0, vim.schedule_wrap(schedule_update))
    end,
  })

  vim.keymap.set({ "n", "i" }, "<Enter>", function()
    local query = parse_query()
    vim.fn.setreg("+", query)
    vim.notify("Copied query!")
  end, { buffer = state.input.buf })

  -- Scrolling keymaps

  vim.keymap.set({ "n", "i" }, "<C-d>", function()
    scroll_results("<C-d>")
  end, { buffer = state.input.buf })

  vim.keymap.set({ "n", "i" }, "<C-u>", function()
    scroll_results("<C-u>")
  end, { buffer = state.input.buf })

  vim.keymap.set({ "n", "i" }, "<C-e>", function()
    scroll_results("<C-e>")
  end, { buffer = state.input.buf })

  vim.keymap.set({ "n", "i" }, "<C-k>", function()
    scroll_results("<C-k>")
  end, { buffer = state.input.buf })
end

local function start_playground()
  local ok, err = pcall(init)
  if not ok then
    vim.notify("jq_query.nvim error: " .. err, vim.log.levels.ERROR)
  end
end
vim.keymap.set("n", "<localleader>q", start_playground, { buffer = 0 })

vim.api.nvim_buf_create_user_command(0, "JqPlayground", start_playground, {})
