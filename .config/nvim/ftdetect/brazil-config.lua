vim.api.nvim_create_autocmd({"BufRead","BufNewFile"}, {
  pattern = { "Config" },
  command = "setfiletype brazil-config",
})
