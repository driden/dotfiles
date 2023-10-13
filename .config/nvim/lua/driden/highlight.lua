vim.api.nvim_create_autocmd({ "TextYankPost" }, {
  pattern = { "*" },
  group = vim.api.nvim_create_augroup("HightlightYank", {
    clear = false,
  }),
  callback = function()
    vim.highlight.on_yank({ higroup = "Visual", timeout = 400 })
  end,
})
