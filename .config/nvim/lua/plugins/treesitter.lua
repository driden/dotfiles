local M = {}

function M.setup()
  require'nvim-treesitter.configs'.setup {
    -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
    highlight = {
      enable = true,
      disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
      additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
    },
    ensure_installed = {'org'}, -- Or run :TSUpdate org
  }
end

return M

