-- TODO: add these
-- https://github.com/rcarriga/nvim-notify
-- https://github.com/folke/noice.nvim
-- https://github.com/folke/lazy.nvim
-- https://github.com/echasnovski/mini.nvim
--
-- WHICH KEY

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  Packer_bootstrap = fn.system({
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
    "AndrewRadev/bufferize.vim",
    "wbthomason/packer.nvim",
    "nvim-lua/plenary.nvim",
    -- THEMES
    "folke/tokyonight.nvim",
    "drewtempelmeyer/palenight.vim",
    "editorconfig/editorconfig-vim",
    "joshdick/onedark.vim",
    "sainnhe/gruvbox-material",
    "NLKNguyen/papercolor-theme",
    "rktjmp/lush.nvim",
    "rebelot/kanagawa.nvim",

    "tpope/vim-surround",

    -- statusbar
    "vim-airline/vim-airline",
    "vim-airline/vim-airline-themes",

    -- filetree
    "kyazdani42/nvim-web-devicons",
    -- Treesitter
    "nvim-treesitter/nvim-treesitter", --{do = 'TSUpdate',}},
    "nvim-treesitter/nvim-treesitter-context",

    -- Indent
    "lukas-reineke/indent-blankline.nvim",

    -- LSP
    "folke/neodev.nvim",
    "neovim/nvim-lspconfig",
    "onsails/lspkind-nvim",
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
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
    --Java
    "mfussenegger/nvim-jdtls",
    "mfussenegger/nvim-dap",

    -- Snippets
    "L3MON4D3/LuaSnip",
    "saadparwaiz1/cmp_luasnip",

    "numToStr/Comment.nvim",

    -- boludeces
    "xiyaowong/nvim-transparent",
  }

  for _, plugin in ipairs(plugins) do
    use(plugin)
  end

  use({
    "nvim-telescope/telescope.nvim",
    tag = "0.1.0",
    requires = { { "nvim-lua/plenary.nvim" } },
  })
  use({
    "phaazon/hop.nvim",
    branch = "v2", -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
    end,
  })
  -- Lua
  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
  })
  -- use({
  --   "nvim-neotest/neotest",
  --   requires = {
  --     "nvim-lua/plenary.nvim",
  --     "nvim-treesitter/nvim-treesitter",
  --     "antoinemadec/FixCursorHold.nvim",
  --     "marilari88/neotest-vitest",
  --   },
  -- })

  use({
    "kyazdani42/nvim-tree.lua",
    commit = "874b7be5d053f1b31f545099d6fcbe8ae81e9e03", -- TOO MANY API CHANGES !!! just pin i
  })

  use({
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
  })

  use({ "rcarriga/nvim-notify" })

  use({ "bluz71/vim-nightfly-colors", as = "nightfly" })

  use({ "nvim-neorg/neorg", run = ":Neorg sync-parsers" })

  if Packer_bootstrap then
    require("packer").sync()
  end
end)
