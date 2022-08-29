TfFormat = function()
  local write_results = function(_, data)
    if data and #data > 1 then
      -- slicing, otherwise there'll always be an increasing number of trailing newlines in the file
      local sliced = vim.list_slice(data, 1, #data - 1)
      vim.api.nvim_buf_set_lines(0, 0, -1, false, sliced)
    end
  end

  local file = vim.api.nvim_buf_get_name(0)
  vim.fn.jobstart({ "terraform", "fmt", "-list=false", "-write=false", "-no-color", file }, {
    stdout_buffered = true,
    on_stdout = write_results,
    on_stderr = write_results,
  })
end

vim.api.nvim_buf_create_user_command(0, "TfFormat", ":lua TfFormat()", {})
-- vim.api.nvim_create_autocmd("BufWritePre", {
--   group = vim.api.nvim_create_augroup("Terraform format", { clear = true }),
--   pattern = "*.tf",
--   callback = format,
-- })