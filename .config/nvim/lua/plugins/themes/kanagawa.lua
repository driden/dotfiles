return {
  'rebelot/kanagawa.nvim',
  config = function()
    require('kanagawa').setup {
      commentStyle = { italic = false },
      keywordStyle = { italic = false },
    }

    vim.cmd [[colorscheme kanagawa-dragon]]
  end,
}
