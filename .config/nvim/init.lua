vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = true

--  see `:help option-list`
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.mouse = "a"
vim.opt.showmode = false
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.list = true
vim.opt.listchars = { lead = "-", tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.inccommand = "split"
vim.opt.cursorline = true
vim.opt.scrolloff = 5
vim.opt.termguicolors = true
vim.opt.wrap = false
vim.opt.ignorecase = true

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.expandtab = true

local toggle_meta_chars = function()
  vim.o.list = not vim.o.list
end

vim.keymap.set("n", "<leader>tm", function()
  toggle_meta_chars()
end)

--  [[ Basic Keymaps ]]
vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/kickstart/init.lua<CR>")
-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Diagnostic keymaps
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
vim.keymap.set("t", "<C-[>", "<C-\\><C-n>", { desc = "Exit terminal mode", silent = true })
vim.keymap.set("t", "<C-h>", "<C-\\><C-N><C-w>h", { desc = "terminal move to pane to the left", silent = true })
vim.keymap.set("t", "<C-j>", "<C-\\><C-N><C-w>j", { desc = "terminal move to pane below", silent = true })
vim.keymap.set("t", "<C-k>", "<C-\\><C-N><C-w>k", { desc = "terminal move to pane up", silent = true })
vim.keymap.set("t", "<C-l>", "<C-\\><C-N><C-w>l", { desc = "terminal move to pane to the right", silent = true })

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

vim.keymap.set("n", "<leader>bk", ":bdelete<CR>", { desc = "Delete this buffer" })
vim.keymap.set("n", "<leader>bK", ":bdelete!<CR>", { desc = "Delete this buffer without saving" })

vim.keymap.set("n", "<Up>", ":resize +2<CR>", { desc = "Horizontal size +2", silent = true })
vim.keymap.set("n", "<Down>", ":resize -2<CR>", { desc = "Horizontal size -2", silent = true })
vim.keymap.set("n", "<Left>", ":vertical resize +3<CR>", { desc = "Vertical size +3", silent = true })
vim.keymap.set("n", "<Right>", ":vertical resize -3<CR>", { desc = "Vertical size -3", silent = true })

-- Visual --
-- Stay in indent mode
vim.keymap.set("v", "<", "<gv", { desc = "remove indentation", silent = true })
vim.keymap.set("v", ">", ">gv", { desc = "indentation", silent = true })

-- Move text up and down
vim.keymap.set({ "n", "v" }, "<A-j>", ":m .+1<CR>==", { desc = "move line down", silent = true })
vim.keymap.set({ "n", "v" }, "<A-k>", ":m .-2<CR>==", { desc = "move line up", silent = true })
-- Paste without saving deleted text to register
vim.keymap.set({ "v", "x" }, "p", '"_dP', { desc = "Paste without copying", silent = true })

vim.keymap.set({ "x", "n" }, "Y", "y$", { desc = "Yank until eol", silent = true })

-- Yank and paste from OS
vim.keymap.set({ "v", "x" }, "<leader>y", '"*y', { desc = "Yank to OS clipboard", silent = true })
vim.keymap.set({ "v", "x" }, "<leader>p", '"*p', { desc = "Paste from OS clipboard", silent = true })

-- Tab
vim.keymap.set("n", "<leader>tn", ":tabNext<CR>", { desc = "Next tab", silent = true })
vim.keymap.set("n", "<leader>tp", ":tabprevious<CR>", { desc = "Prev tab", silent = true })
vim.keymap.set("n", "<leader>tc", ":tabclose<CR>", { desc = "close tab", silent = true })

-- Quick Fix
vim.keymap.set("n", "<leader>qq", ":copen<CR>", { desc = "[Q]uickfix open", silent = true }) -- Open the quickfix window
vim.keymap.set("n", "<leader>qc", ":ccl<CR>", { desc = "[Q]uickfix [C]lose", silent = true }) --  Open it if there are "errors", close it otherwise
vim.keymap.set("n", "<leader>qe", ":cw<CR>", { desc = "[Q]uickfix next error in [W]indow", silent = true }) --  Go to the next error in the window
vim.keymap.set("n", "<leader>qf", ":cnf<CR>", { desc = "[Q]uickfix error under cursor", silent = true }) --Go to error under cursor (if cursor is in quickfix window)
vim.keymap.set("n", "<leader>q.", ":.cc<CR>", { desc = "close tab", silent = true })

--  See `:help lua-guide-autocommands`
-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- [[ Install `lazy.nvim` plugin manager ]]
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    error("Error cloning lazy.nvim:\n" .. out)
  end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- NOTE: Here is where you install your plugins.
require("lazy").setup({
  "tpope/vim-unimpaired",
  "tpope/vim-surround",
  -- 'tpope/vim-sleuth', -- Detect tabstop and shiftwidth automatically

  { import = "plugins" },

  -- LSP Plugins
  {
    -- `lazydev` configures Lua LSP for your Neovim config, runtime and plugins
    -- used for completion, annotations and signatures of Neovim apis
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = "luvit-meta/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta", lazy = true },

  { -- Autoformat
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>f",
        function()
          require("conform").format({ async = true, lsp_format = "fallback" })
        end,
        mode = "",
        desc = "[F]ormat buffer",
      },
    },
    opts = {
      notify_on_error = false,
      format_on_save = function(bufnr)
        -- Disable "format_on_save lsp_fallback" for languages that don't
        -- have a well standardized coding style. You can add additional
        -- languages here or re-enable it for the disabled ones.
        local disable_filetypes = { c = true, cpp = true, java = true, typecript = true }
        local lsp_format_opt
        if disable_filetypes[vim.bo[bufnr].filetype] then
          lsp_format_opt = "never"
        else
          lsp_format_opt = "fallback"
        end
        return {
          timeout_ms = 500,
          lsp_format = lsp_format_opt,
        }
      end,
      formatters_by_ft = {
        lua = { "stylua" },
        -- Conform can also run multiple formatters sequentially
        -- python = { "isort", "black" },
        --
        -- You can use 'stop_after_first' to run the first available formatter from the list
        -- javascript = { "prettierd", "prettier", stop_after_first = true },
      },
    },
  },

  -- Highlight todo, notes, etc in comments
  {
    "folke/todo-comments.nvim",
    event = "VimEnter",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = { signs = false },
  },
  { -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    main = "nvim-treesitter.configs", -- Sets main module to use for opts
    -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
    opts = {
      ensure_installed = {
        "asm",
        "bash",
        "c",
        "diff",
        "html",
        "lua",
        "luadoc",
        "markdown",
        "markdown_inline",
        "query",
        "vim",
        "vimdoc",
      },
      -- Autoinstall languages that are not installed
      auto_install = true,
      highlight = {
        enable = true,
        -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
        --  If you are experiencing weird indenting issues, add the language to
        --  the list of additional_vim_regex_highlighting and disabled languages for indent.
        additional_vim_regex_highlighting = { "ruby" },
      },
      indent = { enable = true, disable = { "ruby" } },
    },
    -- There are additional nvim-treesitter modules that you can use to interact
    -- with nvim-treesitter. You should go explore a few and see what interests you:
    --
    --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
    --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
    --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
  },
  {
    "xiyaowong/transparent.nvim",
    config = function()
      require("transparent").setup({
        extra_groups = { -- table/string: additional groups that should be cleared
          -- In particular, when you set it to 'all', that means all available groups

          -- example of akinsho/nvim-bufferline.lua
          "BufferLineTabClose",
          "BufferlineBufferSelected",
          "BufferLineFill",
          "BufferLineBackground",
          "BufferLineSeparator",
          "BufferLineIndicatorSelected",
        },
        exclude_groups = {}, -- table: groups you don't want to clear
      })
    end,
  },
  "mfussenegger/nvim-jdtls",
  -- require 'kickstart.plugins.debug',
  -- require("plugins.indent"),
  -- require 'plugins.lint',
  -- require("plugins.tree"),
}, {
  ui = {
    -- If you are using a Nerd Font: set icons to an empty table which will use the
    -- default lazy.nvim defined Nerd Font icons, otherwise define a unicode icons table
    icons = vim.g.have_nerd_font and {} or {
      cmd = "⌘",
      config = "🛠",
      event = "📅",
      ft = "📂",
      init = "⚙",
      keys = "🗝",
      plugin = "🔌",
      runtime = "💻",
      require = "🌙",
      source = "📄",
      start = "🚀",
      task = "📌",
      lazy = "💤 ",
    },
  },
})

require("usercommands")
-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
