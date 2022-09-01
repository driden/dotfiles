local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
  vim.cmd([[packadd packer.nvim]])
end

require("packer").startup(function(use)
  local plugins = {
    "wbthomason/packer.nvim",
    "nvim-lua/plenary.nvim",
    -- THEMES
    "drewtempelmeyer/palenight.vim",
    "editorconfig/editorconfig-vim",
    "joshdick/onedark.vim",
    "sainnhe/gruvbox-material",
    "NLKNguyen/papercolor-theme",
    "rktjmp/lush.nvim",
    "rebelot/kanagawa.nvim",

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

    -- Indent
    "lukas-reineke/indent-blankline.nvim",

    -- LSP
    "neovim/nvim-lspconfig",
    "onsails/lspkind-nvim",
    "williamboman/nvim-lsp-installer",
    "jose-elias-alvarez/null-ls.nvim",
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-omni",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    "hrsh7th/cmp-buffer",
    --
    -- DAP
    "mfussenegger/nvim-dap",
    "rcarriga/nvim-dap-ui",

    --Java
    "mfussenegger/nvim-jdtls",

    -- Snippets
    "L3MON4D3/LuaSnip"
  }

  for _, plugin in ipairs(plugins) do
    use(plugin)
  end

  use({ "nvim-telescope/telescope.nvim", tag = "0.1.0", requires = { { "nvim-lua/plenary.nvim" } } })
  use({
    "phaazon/hop.nvim",
    branch = "v2", -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
    end,
  })

  if packer_bootstrap then
    require("packer").sync()
  end
end)
