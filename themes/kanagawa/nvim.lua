-- themes/kanagawa/nvim.lua
return {
  plugin = "kanagawa.nvim",
  colorscheme = "kanagawa-wave",
  setup = function()
    require("kanagawa").setup({
      commentStyle = { italic = false },
      keywordStyle = { italic = false },
    })
  end,
}
