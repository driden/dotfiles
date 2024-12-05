vim.api.nvim_create_autocmd({"BufRead","BufNewFile"}, {
  pattern = { "*.tex" },
  command = "setfiletype latex",
})
