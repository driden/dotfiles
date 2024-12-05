vim.api.nvim_buf_create_user_command(0, "ConvertChecks", function()
  local line, _ = unpack(vim.api.nvim_win_get_cursor(0))
  local old = vim.api.nvim_get_current_line()
  local replace = '\\1- keyCheck: $.\\2#\\1  valueCheck:\\3'
  local search = "\\(\\s*\\)\\(\\p*\\):\\(.*\\)"
  local lines = vim.split(vim.fn.substitute(old, search, replace, ""), "#")

  vim.api.nvim_buf_set_lines(0, line - 1, line, false, lines)
end, {})
