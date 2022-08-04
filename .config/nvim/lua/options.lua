local M = {}

M.toggle_meta_chars = function()
  vim.o.list = not vim.o.list
end

function M.setup()
  local opts = {
    --o.termguicolors = true Seems to be broken now
    autoindent = true,
    backspace = "indent,eol,start",
    backup = false,
    clipboard = "unnamedplus",
    cmdheight = 2,
    compatible = false,
    conceallevel = 2, -- so that `` is visible in markdown files
    concealcursor = "nc",
    cursorline = true,
    expandtab = true,
    fileencoding = "utf-8", -- the encoding written to a file
    hidden = true,
    hlsearch = false,
    ignorecase = true,
    laststatus = 3,
    list = false,
    listchars = "lead:-,tab:>.,trail:~,eol:↴",
    number = true,
    relativenumber = true,
    shiftwidth = 2,
    showmode = false,
    signcolumn = "yes",
    smartindent = true,
    splitbelow = true,
    splitright = true,
    swapfile = false,
    tabstop = 2,
    updatetime = 300,
    wrap = false,
    writebackup = false,
  }

  for k, v in pairs(opts) do
    --  vim.pretty_print(k)
    vim.o[k] = v
  end
  vim.opt.rtp:append("/usr/loca/bin/fzf")
  vim.opt.shortmess:append("c")

  -- neovide
  if vim.fn.exists("g:neovide") then
    vim.opt.guifont = "UbuntuMono Nerd Font Mono:h20"
  end

end

return M
