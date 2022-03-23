local M = {}

M.toggle_meta_chars = function()
	vim.o.list = not vim.o.list
end

function M.setup()
	vim.o.autoindent = true
	vim.o.background = "dark"
	vim.o.backspace = "indent,eol,start"
	vim.o.backup = false
	vim.o.clipboard = "unnamedplus"
	vim.o.cmdheight = 2
	vim.o.compatible = false
	vim.o.cursorline = true
	vim.o.expandtab = true
	vim.o.hidden = true
	vim.o.hlsearch = false
	vim.o.ignorecase = true
	vim.o.list = true
	vim.o.listchars = "tab:>·,trail:~,eol:↴"
	vim.o.relativenumber = true
	vim.o.shiftwidth = 2
	vim.o.showmode = false
	vim.o.signcolumn = "yes"
	vim.o.tabstop = 4
	--vim.o.termguicolors = true
	vim.o.updatetime = 300
	vim.o.wrap = false
	vim.o.writebackup = false
	vim.opt.rtp:append("/usr/loca/bin/fzf")
	vim.opt.shortmess:append("c")
end

return M
