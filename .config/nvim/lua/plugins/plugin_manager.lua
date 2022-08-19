local M = {}

function M.setup()
  local paq = require("paq")
  local plugins = {
    "nvim-lua/plenary.nvim",
    -- THEMES
    "drewtempelmeyer/palenight.vim",
    "editorconfig/editorconfig-vim",
    "joshdick/onedark.vim",
    "sainnhe/gruvbox-material",
    "NLKNguyen/papercolor-theme",
    "metalelf0/jellybeans-nvim",
    "rktjmp/lush.nvim",

    -- fzf
    "junegunn/fzf", -- { 'do',: { -> fzf#install() } }
    "junegunn/fzf.vim",
    -- git
    "tpope/vim-fugitive",
    "tpope/vim-surround",
    -- statusbar
    "vim-airline/vim-airline",
    "vim-airline/vim-airline-themes",
    --term
    "akinsho/toggleterm.nvim",

    -- filetree
    "kyazdani42/nvim-web-devicons",
    "kyazdani42/nvim-tree.lua",
    -- Treesitter
    "nvim-treesitter/nvim-treesitter", --{do = 'TSUpdate',}},

    -- LSP
    "neovim/nvim-lspconfig",
    "onsails/lspkind-nvim",
    "williamboman/nvim-lsp-installer",
    "jose-elias-alvarez/null-ls.nvim",
    "mfussenegger/nvim-jdtls",
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-vsnip",
    "hrsh7th/vim-vsnip",
    -- DAP
    "mfussenegger/nvim-dap",
    "rcarriga/nvim-dap-ui",
  }

  paq(plugins)
end

return M
