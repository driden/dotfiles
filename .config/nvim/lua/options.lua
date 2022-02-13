local set = vim.opt

set.hidden = true
set.backup = false
set.writebackup = false
set.list = true
set.cmdheight = 2
set.shortmess:append("c")
set.updatetime = 300
set.signcolumn = "yes"
set.clipboard = "unnamedplus"
set.cursorline = true
set.wrap = false
set.relativenumber = true
set.compatible = false
set.ignorecase = true
set.autoindent = true
set.shiftwidth = 2
set.tabstop = 4
set.expandtab = true
set.incsearch  = true
set.backspace = "indent,eol,start"
set.showmode = false
set.rtp:append("/usr/loca/bin/fzf")

set.listchars = "tab:>·,trail:~"
    --set.listchars = "tab:>,trail:~,extends:>,precedes:<,space:."


local function set_char_list(show)
  if (show) then
    set.listchars = "tab:>·,trail:~"
  else
    set.listchars = ""
  end
end

function TOGGLE_SHOW_CHAR_LIST()
  local show_chars = #(set.listchars) > 0
  set_char_list(show_chars)
end

