return {
  'kyazdani42/nvim-tree.lua',
  init = function()
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1
    vim.keymap.set({ 'n' }, '<leader>e', ':NvimTreeToggle<CR>', { silent = true })
  end,
  config = function()
    local api = require 'nvim-tree.api'
    require('nvim-tree').setup {
      -- BEGIN_DEFAULT_OPTS
      hijack_cursor = true,
      on_attach = function()
        vim.keymap.set({ 'n' }, '<Tab>', api.node.open.preview, { silent = true })
        vim.keymap.set({ 'n' }, '<Enter>', api.node.open.preview, { silent = true })
        vim.keymap.set({ 'n' }, '-', api.tree.change_root_to_parent, { silent = true })
      end,
      prefer_startup_root = true,
      sync_root_with_cwd = true,
      select_prompts = false,
      update_focused_file = {
        enable = true,
      },
    }
  end,
}
