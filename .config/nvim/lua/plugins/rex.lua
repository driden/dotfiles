-- Ctrl-h/j/k/l across Neovim windows and Rex panes (Rex half: ~/.config/rex/rex-navigator.lua).
-- Pinned to the audited commit; review the diff before bumping.
return {
  "baggiponte/rex-navigator.nvim",
  commit = "6dc3f2fd3e3abe4738b3f7ba4185d3a5fb1c247f",
  main = "rex-navigator",
  lazy = false,
  -- Rex 0.1.0-preview.23 sets REX_BLOCK but not REX_SESSION, which the plugin keys on.
  -- A block id also names its session (`rex do -s $REX_BLOCK` works), so borrow it.
  init = function()
    if (vim.env.REX_SESSION or "") == "" and (vim.env.REX_BLOCK or "") ~= "" then
      vim.env.REX_SESSION = vim.env.REX_BLOCK
    end
  end,
  opts = {},
}
