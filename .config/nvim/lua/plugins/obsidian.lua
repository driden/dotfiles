local notes_dirs = { "~/Documents/Notes", "~/Documents/Notes/personal", "~/notes" }

local workspaces = vim
  .iter(notes_dirs)
  :map(function(dir)
    return vim.fn.expand(dir)
  end)
  :map(function(curr_dir)
    for dir, type in vim.fs.dir(curr_dir) do
      if type == "directory" then
        -- https://www.lua.org/manual/5.1/manual.html
        -- 5.4.1 patterns
        -- filter out hidden folders
        if not dir:match("%p") then
          local candidate = curr_dir .. "/" .. dir
          -- if candidate has an `.obsidian` folder then we can add it
          if vim.uv.fs_stat(candidate .. "/.obsidian") then
            local name = candidate:match("([^/]+)$")
            return { name = name, path = candidate }
          end
        end
      end
    end
  end)
  :totable()

return {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    workspaces = workspaces,
  },
}
