vim.opt_local.conceallevel = 2

local util = require("driden.util")

---@class MdEditing
---@field heading MdEditing.Heading
---@field list MdEditing.List
local M = {}

---------------------------------------------------------------------------
-- M.heading
---------------------------------------------------------------------------

---@class MdEditing.Heading
M.heading = {}

--- Get the heading node, marker node, and level at a given row
---@param row? integer 0-based row (defaults to cursor row)
---@return TSNode?, TSNode?, integer?
function M.heading.get(row)
  row = row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
  local node = vim.treesitter.get_node({ pos = { row, 0 } })
  node = util.find_ancestor(node, "atx_heading")
  if not node then return nil, nil, nil end
  ---@type TSNode
  local marker = node:child(0)
  local text = vim.treesitter.get_node_text(marker, 0)
  ---@type integer
  local level = #text:match("^(#+)")
  return node, marker, level
end

--- Set heading level at a given row
---@param row integer 0-based row
---@param new_level integer 0 = remove heading, 1-6 = set level
function M.heading.set(row, new_level)
  local _, marker, level = M.heading.get(row)
  if not marker then
    if new_level > 0 then
      vim.api.nvim_buf_set_text(0, row, 0, row, 0, { string.rep("#", new_level) .. " " })
    end
    return
  end
  if new_level == level then return end
  local sr, sc, _, ec = marker:range()
  if new_level < 1 then
    vim.api.nvim_buf_set_text(0, sr, sc, sr, ec + 1, { "" })
  else
    vim.api.nvim_buf_set_text(0, sr, sc, sr, ec, { string.rep("#", new_level) })
  end
end

--- Promote heading at row (decrease level, remove at level 1)
---@param row? integer 0-based row
function M.heading.promote(row)
  row = row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
  local _, _, level = M.heading.get(row)
  if not level then return end
  M.heading.set(row, level - 1)
end

--- Demote heading at row (increase level, create at level 1 if not a heading)
---@param row? integer 0-based row
function M.heading.demote(row)
  row = row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
  local _, _, level = M.heading.get(row)
  M.heading.set(row, level and math.min(level + 1, 6) or 1)
end

--- Apply fn to each heading in a range (bottom-to-top to avoid row shifts)
---@param start_row integer 0-based inclusive
---@param end_row integer 0-based inclusive
---@param fn fun(row: integer)
function M.heading.for_range(start_row, end_row, fn)
  for row = end_row, start_row, -1 do
    if M.heading.get(row) then
      fn(row)
    end
  end
end

---------------------------------------------------------------------------
-- M.list
---------------------------------------------------------------------------

---@class MdEditing.List
---@field markers string[]
M.list = {}

--- Cycle order for list markers
M.list.markers = { "-", "*", "1." }

--- Get the list_item node, marker node, and normalized marker string at a row
---@param row? integer 0-based row (defaults to cursor row)
---@return TSNode?, TSNode?, string?
function M.list.get_item(row)
  row = row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
  local node = vim.treesitter.get_node({ pos = { row, 0 } })
  node = util.find_ancestor(node, "list_item")
  if not node then return nil, nil, nil end
  ---@type TSNode
  local marker = node:child(0)
  local text = vim.trim(vim.treesitter.get_node_text(marker, 0))
  -- Normalize: "3." or "12." -> "1.", "-" -> "-", "*" -> "*"
  local normalized = text:match("^%d+%.") and "1." or text
  return node, marker, normalized
end

--- Get the parent list node for a list_item
---@param item TSNode
---@return TSNode?
function M.list.get_parent_list(item)
  return util.find_ancestor(item:parent(), "list") or item:parent()
end

--- Get all direct list_item children of a list node
---@param list_node TSNode
---@return TSNode[]
function M.list.get_items(list_node)
  ---@type TSNode[]
  local items = {}
  for i = 0, list_node:named_child_count() - 1 do
    local child = list_node:named_child(i)
    if child and child:type() == "list_item" then
      items[#items + 1] = child
    end
  end
  return items
end

--- Cycle the entire list containing the cursor item
---@param direction 1|-1
function M.list.cycle(direction)
  local item, _, current = M.list.get_item()
  if not item or not current then return end
  local list_node = M.list.get_parent_list(item)
  if not list_node then return end

  -- Find current position in markers cycle
  local idx = 1
  for i, m in ipairs(M.list.markers) do
    if m == current then idx = i break end
  end
  local next_idx = ((idx - 1 + direction) % #M.list.markers) + 1
  local next_marker = M.list.markers[next_idx]

  -- Replace all items in reverse order
  local items = M.list.get_items(list_node)
  for i = #items, 1, -1 do
    local child_marker = items[i]:child(0)
    if child_marker then
      local sr, sc, _, ec = child_marker:range()
      local replacement
      if next_marker == "1." then
        replacement = i .. ". "
      else
        replacement = next_marker .. " "
      end
      vim.api.nvim_buf_set_text(0, sr, sc, sr, ec, { replacement })
    end
  end
end

--- Cycle all unique lists within a row range
---@param start_row integer 0-based inclusive
---@param end_row integer 0-based inclusive
---@param direction 1|-1
function M.list.cycle_range(start_row, end_row, direction)
  ---@type table<string, boolean>
  local seen = {}
  ---@type { list: TSNode, item: TSNode, marker: string }[]
  local entries = {}

  for row = start_row, end_row do
    local item, _, current = M.list.get_item(row)
    if item and current then
      local list_node = M.list.get_parent_list(item)
      if list_node then
        local id = tostring(list_node:id())
        if not seen[id] then
          seen[id] = true
          entries[#entries + 1] = { list = list_node, item = item, marker = current }
        end
      end
    end
  end

  -- Process in reverse order of appearance
  for i = #entries, 1, -1 do
    local entry = entries[i]
    local idx = 1
    for j, m in ipairs(M.list.markers) do
      if m == entry.marker then idx = j break end
    end
    local next_idx = ((idx - 1 + direction) % #M.list.markers) + 1
    local next_marker = M.list.markers[next_idx]

    local items = M.list.get_items(entry.list)
    for k = #items, 1, -1 do
      local child_marker = items[k]:child(0)
      if child_marker then
        local sr, sc, _, ec = child_marker:range()
        local replacement
        if next_marker == "1." then
          replacement = k .. ". "
        else
          replacement = next_marker .. " "
        end
        vim.api.nvim_buf_set_text(0, sr, sc, sr, ec, { replacement })
      end
    end
  end
end

---------------------------------------------------------------------------
-- Keymaps
---------------------------------------------------------------------------

-- Heading: normal mode
vim.keymap.set("n", "]]", M.heading.demote, { buffer = true, desc = "Demote heading" })
vim.keymap.set("n", "[[", M.heading.promote, { buffer = true, desc = "Promote heading" })

-- Heading: visual mode
vim.keymap.set("v", "]]", function()
  local sr, er = util.visual_range()
  M.heading.for_range(sr, er, M.heading.demote)
end, { buffer = true, desc = "Demote headings in selection" })

vim.keymap.set("v", "[[", function()
  local sr, er = util.visual_range()
  M.heading.for_range(sr, er, M.heading.promote)
end, { buffer = true, desc = "Promote headings in selection" })

-- List: normal mode
vim.keymap.set("n", "]l", function() M.list.cycle(1) end, { buffer = true, desc = "Cycle list marker forward" })
vim.keymap.set("n", "[l", function() M.list.cycle(-1) end, { buffer = true, desc = "Cycle list marker backward" })

-- List: visual mode
vim.keymap.set("v", "]l", function()
  local sr, er = util.visual_range()
  M.list.cycle_range(sr, er, 1)
end, { buffer = true, desc = "Cycle list markers forward in selection" })

vim.keymap.set("v", "[l", function()
  local sr, er = util.visual_range()
  M.list.cycle_range(sr, er, -1)
end, { buffer = true, desc = "Cycle list markers backward in selection" })

---------------------------------------------------------------------------
-- Obsidian
---------------------------------------------------------------------------

vim.keymap.set(
  { "v", "x" },
  "<localleader>e",
  ":Obsidian extract_note<CR>",
  { silent = true, buffer = true, desc = "Extract to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>l",
  ":Obsidian link<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set(
  { "v", "x" },
  "<localleader>L",
  ":Obsidian link_new<CR>",
  { silent = true, buffer = true, desc = "Link selected text to note" }
)

vim.keymap.set("n", "<localleader>n", ":Obsidian new<CR>", { silent = true, buffer = true, desc = "New note" })
vim.keymap.set(
  "n",
  "<localleader>N",
  ":Obsidian new_from_template<CR>",
  { silent = true, buffer = true, desc = "New note from Template" }
)
vim.keymap.set(
  "n",
  "<localleader>f",
  ":Obsidian quick_switch<CR>",
  { silent = true, buffer = true, desc = "Find Note" }
)
vim.keymap.set("n", "<localleader>T", ":Obsidian today<CR>", { silent = true, buffer = true, desc = "Today's note" })
vim.keymap.set(
  "n",
  "<localleader>t",
  ":Obsidian template<CR>",
  { silent = true, buffer = true, desc = "Insert Template" }
)
vim.keymap.set(
  "n",
  "<localleader>w",
  ":Obsidian workspace<CR>",
  { silent = true, buffer = true, desc = "Open workspace" }
)
