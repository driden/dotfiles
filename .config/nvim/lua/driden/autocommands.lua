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

-- recording macro
vim.api.nvim_create_autocmd({ "RecordingEnter" }, {
  callback = function()
    vim.o.cmdheight = 1
  end
})

vim.api.nvim_create_autocmd({ "RecordingLeave" }, {
  callback = function()
    vim.o.cmdheight = 0
  end
})
