local notes_dir = "~/Documents/Notes"
local notes_expanded = vim.fn.expand(notes_dir)
local workspaces = {}
for dir, type in vim.fs.dir(notes_expanded) do
  if type == "directory" then
    -- https://www.lua.org/manual/5.1/manual.html
    -- 5.4.1 patterns
    -- filter out hidden folders
    if not dir:match("%p") then
      local candidate = notes_expanded .. "/" .. dir
      -- if candidate has an `.obsidian` folder then we can add it
      if vim.uv.fs_stat(candidate .. "/.obsidian") then
        local name = candidate:match("([^/]+)$")
        table.insert(workspaces, { name = name, path = candidate })
      end
    end
  end
end

local personal_vault = vim.fn.expand("~/Documents/Notes/Markdown")
local work_vault = vim.fn.expand("~/Documents/Notes/work")
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
