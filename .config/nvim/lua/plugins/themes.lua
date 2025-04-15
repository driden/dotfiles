local theme = "cyberdream"

return {
  {
    "scottmckendry/cyberdream.nvim",
    lazy = false,
    priority = 1000,
    enabled = theme == "cyberdream",
    config = function()
      vim.cmd.colorscheme("cyberdream")
    end,
  },
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    enabled = theme == "gruvbox",
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
      vim.cmd.colorscheme("gruvbox")
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    enabled = theme == "kanagawa",
    config = function()
      require("kanagawa").setup({
        commentStyle = { italic = false },
        keywordStyle = { italic = false },
      })

      vim.cmd.colorscheme("kanagawa-dragon")
    end,
  },
  {
    "folke/tokyonight.nvim",
    enalbed = theme == "tokyonight",
    config = function()
      vim.cmd.colorscheme("tokyonight")
    end,
  },
}
