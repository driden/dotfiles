local personal_vault = vim.fn.expand("~/Documents/Notes/personal/Markdown")
local work_vault = vim.fn.expand("~/Documents/Notes/work")
return {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  -- ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  event = {
    "BufReadPre " .. personal_vault .. "/*.md",
    "BufNewFile " .. personal_vault .. "/*.md",
    "BufReadPre " .. work_vault .. "/*.md",
    "BufNewFile " .. work_vault .. "/*.md",
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    workspaces = {
      {
        name = "personal",
        path = "~/Documents/Notes/personal/Markdown",
      },
      {
        name = "work",
        path = "~/Documents/Notes/work/",
      },
    },

    -- see below for full list of options 👇
  },
}
