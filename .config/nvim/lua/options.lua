local M = {}
-- local M = {{show_chars = true}}
--
-- function M.toggle_meta_chars()
--    M.show_chars = ~M.show_chars
--    vim.opt.list = ~M.show_chars
-- end
function M.setup()
	vim.opt.autoindent = true
	vim.opt.backspace = "indent,eol,start"
	vim.opt.backup = false
	vim.opt.clipboard = "unnamedplus"
	vim.opt.cmdheight = 2
	vim.opt.compatible = false
	vim.opt.cursorline = true
	vim.opt.expandtab = true
	vim.opt.hidden = true
	vim.opt.ignorecase = true
	vim.opt.incsearch = true
	vim.opt.list = true
	vim.opt.listchars = "tab:>·,trail:~"
	vim.opt.relativenumber = true
	vim.opt.rtp:append("/usr/loca/bin/fzf")
	vim.opt.shiftwidth = 2
	vim.opt.shortmess:append("c")
	vim.opt.showmode = false
	vim.opt.signcolumn = "yes"
	vim.opt.tabstop = 4
	vim.opt.updatetime = 300
	vim.opt.wrap = false
	vim.opt.writebackup = false
end

return M
