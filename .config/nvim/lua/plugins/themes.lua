local theme = require("driden.theme").theme

return {
  {
    "scottmckendry/cyberdream.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      if theme == "cyberdream" then
        vim.cmd.colorscheme("cyberdream")
      end
    end,
  },
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      require("gruvbox").setup({
        italic = {
          strings = false,
          comments = true,
          folds = false,
          operators = false,
          emphasis = false,
        },
      })

      if theme == "gruvbox" then
        vim.cmd.colorscheme("gruvbox")
      end
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    config = function()
      require("kanagawa").setup({
        commentStyle = { italic = false },
        keywordStyle = { italic = false },
      })

      if theme == "kanagawa" then
        vim.cmd.colorscheme("kanagawa-dragon")
      end
    end,
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    config = function()
      if theme == "tokyonight" then
        vim.cmd.colorscheme("tokyonight-moon")
      end
    end,
  },
}
