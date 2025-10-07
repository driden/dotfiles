local notes_dirs = { "~/Documents/Notes", "~/Documents/Notes/personal", "~/notes" }

local workspaces = vim
  .iter(notes_dirs)
  :map(function(dir)
    return { name = dir, path = vim.fn.expand(dir) }
  end)
  :map(function(data)
    if vim.uv.fs_stat(data.path .. "/.obsidian") then
      return data
    end
  end)
  :totable()

return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  ---@module 'obsidian'
  ---@type obsidian.config
  opts = {
    legacy_commands = false,
    workspaces = workspaces,
    note_path_func = function(spec)
      return spec.title .. ".md"
    end,
    completion = {
      nvim_cmp = true,
      min_chars = 2,
    },
    footer = {
      enabled = false,
    },
  },
  enabled = true,
}
