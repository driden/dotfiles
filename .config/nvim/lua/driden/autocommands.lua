-- switch working directory to provided dir
vim.api.nvim_create_autocmd({ "VimEnter" }, {
  callback = function()
    local dir = vim.fn.argv()[1]
    -- 0 is FALSE in nvim
    if vim.fn.isdirectory(dir) ~= 0 then
      vim.cmd({ cmd = "cd", args = { dir } })
    end
  end
})
