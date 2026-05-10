return {
  "rcarriga/nvim-notify",
  event = "VeryLazy",
  config = function()
    vim.opt.termguicolors = true
    vim.notify = require("notify")
  end,
}
