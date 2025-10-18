vim.api.nvim_buf_create_user_command(0, "Jq", "%!jq", {})
vim.api.nvim_buf_create_user_command(0, "JqSortKeys", "%!jq --sort-keys", {})
if vim.version().minor < 10 then
  vim.notify("jq_query.nvim requires Neovim 0.10 or newer.", vim.log.levels.ERROR)
  return
end

-- Create a floating window
local function create_float(opts)
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    style = "minimal",
    border = "rounded",
    width = opts.width or math.floor(vim.o.columns * 0.8),
    height = opts.height or 5,
    row = opts.row or math.floor(vim.o.lines * 0.2),
    col = opts.col or math.floor(vim.o.columns * 0.1),
  })
  return buf, win
end

-- Run jq and update output buffer
local function run_jq(filter, source_buf, result_buf)
  local json_lines = vim.api.nvim_buf_get_lines(source_buf, 0, -1, false)
  local json_input = table.concat(json_lines, "\n")

  if filter == "" then
    vim.api.nvim_buf_set_lines(result_buf, 0, -1, false, { "<empty query>" })
    return
  end

  local result = vim.system({ "jq", filter }, { stdin = json_input }):wait()

  if result.code ~= 0 then
    local err_msg = vim.trim(result.stderr)
    -- Split the error message into lines
    local err_lines = vim.split("Error: " .. err_msg, "\n", { plain = true })
    vim.api.nvim_buf_set_lines(result_buf, 0, -1, false, err_lines)
  else
    local output = vim.split(vim.trim(result.stdout), "\n")
    vim.api.nvim_buf_set_lines(result_buf, 0, -1, false, output)
  end
end

local function open()
  local win_refs = { input_win = nil, result_win = nil } -- Store references to the windows for refocusing
  local source_buf = vim.api.nvim_get_current_buf()

  -- Create input window (query)
  local input_buf, input_win = create_float({ height = 1, row = 2 })
  vim.api.nvim_buf_set_option(input_buf, "buftype", "prompt")
  vim.fn.prompt_setprompt(input_buf, "jq> ")
  win_refs.input_win = input_win

  -- Create output window (results)
  local result_buf, result_win = create_float({ height = 15, row = 5 })
  win_refs.result_win = result_win

  -- Function to close both windows
  local function close_windows()
    if vim.api.nvim_win_is_valid(input_win) then
      vim.api.nvim_win_close(input_win, true)
    end
    if vim.api.nvim_win_is_valid(result_win) then
      vim.api.nvim_win_close(result_win, true)
    end
  end

  -- Map 'q' to close both windows
  for _, buf in ipairs({ input_buf, result_buf }) do
    vim.keymap.set("n", "q", close_windows, { buffer = buf, nowait = true })
  end

  -- Set up clipboard copy (<localleader>c)
  vim.keymap.set("n", "<localleader>c", function()
    local line = vim.api.nvim_get_current_line()
    local query = line:gsub("^jq>%s*", "")
    vim.fn.setreg("+", query)
    vim.notify("Copied jq query to clipboard: " .. query)
  end, { buffer = input_buf })

  -- Callback on input change (live update)
  local timer = vim.uv.new_timer()
  local last_query = ""
  local function schedule_update()
    local line = vim.api.nvim_get_current_line():gsub("^jq>%s*", "")
    if line ~= last_query then
      last_query = line
      run_jq(line, source_buf, result_buf)
    end
  end

  vim.api.nvim_create_autocmd("TextChangedI", {
    buffer = input_buf,
    callback = function()
      timer:stop()
      timer:start(200, 0, vim.schedule_wrap(schedule_update))
    end,
  })

  vim.api.nvim_create_autocmd("TextChanged", {
    buffer = input_buf,
    callback = function()
      timer:stop()
      timer:start(200, 0, vim.schedule_wrap(schedule_update))
    end,
  })

  -- Focus on input
  vim.api.nvim_set_current_win(input_win)

  -- Key mappings to refocus windows
  vim.keymap.set("n", "<localleader>i", function()
    if win_refs.input_win and vim.api.nvim_win_is_valid(win_refs.input_win) then
      vim.api.nvim_set_current_win(win_refs.input_win)
    else
      vim.notify("Input window is not available.", vim.log.levels.WARN)
    end
  end, { buffer = source_buf })

  vim.keymap.set("n", "<localleader>o", function()
    if win_refs.result_win and vim.api.nvim_win_is_valid(win_refs.result_win) then
      vim.api.nvim_set_current_win(win_refs.result_win)
    else
      vim.notify("Output window is not available.", vim.log.levels.WARN)
    end
  end, { buffer = source_buf })
end

-- Create user command
vim.api.nvim_create_user_command("JqQuery", function()
  local ok, err = pcall(open)
  if not ok then
    vim.notify("jq_query.nvim error: " .. err, vim.log.levels.ERROR)
  end
end, {})

vim.keymap.set("n", "Q", function()
  local ok, err = pcall(open)
  if not ok then
    vim.notify("jq_query.nvim error: " .. err, vim.log.levels.ERROR)
  end
end, { buffer = source_buf })
