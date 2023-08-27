local M = {}

M.toggle_meta_chars = function()
    vim.o.list = not vim.o.list
end

function M.setup()
    local opts = {
        termguicolors = true,
        autoindent = true,
        backspace = "indent,eol,start",
        backup = false,
        clipboard = "",
        cmdheight = 1,
        compatible = false,
        conceallevel = 0, -- Might want to change this in specific filetypes
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
        scrolloff = 5,
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
    vim.opt.shortmess:append("cmnwxsI")

    vim.o.completeopt = "menu,menuone,noselect"

    vim.notify = require("notify")
end

return M
