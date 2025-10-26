return {
  "yetone/avante.nvim",
  event = "VeryLazy",
  version = false, -- Never set this value to "*"! Never!
  config = function()
    require("avante").setup({
      -- add any opts here
      -- for example
      input = {
        provider = "native",
      },
      provider = "ollama",
      providers = {
        ollama = {
          model = "qwen/qwen3-4b-thinking-2507",
          is_env_set = require("avante.providers.ollama").check_endpoint_alive,
        },
      },
    })
  end,
  -- opts = {
  --   -- add any opts here
  --   -- for example
  --   input = {
  --     provider = "native",
  --   },
  --   provider = "ollama",
  --   providers = {
  --     ollama = {
  --       model = "qwen/qwen3-4b-thinking-2507",
  --       is_env_set = require("avante.providers.ollama").check_endpoint_alive,
  --     },
  --   },
  -- },
  build = "make",
  cond = vim.fn.isdirectory(vim.fn.expand("~/brazil-pkg-cache")) == 0,
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "stevearc/dressing.nvim",
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    {
      -- support for image pasting
      "HakonHarnes/img-clip.nvim",
      event = "VeryLazy",
      opts = {
        -- recommended settings
        default = {
          embed_image_as_base64 = false,
          prompt_for_file_name = false,
          drag_and_drop = {
            insert_mode = true,
          },
          -- required for Windows users
          use_absolute_path = true,
        },
      },
    },
    {
      "zbirenbaum/copilot.lua",
      cmd = "Copilot",
      event = "InsertEnter",
      config = function()
        require("copilot").setup({
          server_opts_overrides = {
            settings = {
              -- offset_encoding = "utf-16",
              telemetry = {
                telemetryLevel = "none",
              },
            },
          },
        })
      end,
    },
  },
}
