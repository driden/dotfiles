return {
  "rcarriga/nvim-notify",
  config = function()
    vim.opt.termguicolors = true
    vim.notify = require("notify")
  end,
}
