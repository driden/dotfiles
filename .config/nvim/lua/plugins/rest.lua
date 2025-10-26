return {
  {
    "mistweaverco/kulala.nvim",
    keys = {
      { "<localleader>s", "<cmd>require('kulala').run()<cr>", desc = "Send request" },
      { "<localleader>a", "<cmd>require('kulala').run_all()<cr>", desc = "Send all requests" },
      { "<localleader>b", "<cmd>require('kulala').scratchpad()<cr>", desc = "Open scratchpad" },
    },
    ft = { "http", "rest" },
    opts = {
      -- your configuration comes here
      global_keymaps = false,
      global_keymaps_prefix = "<leader>R",
      kulala_keymaps_prefix = "",
      ui = {
        win_opts = {
          wo = { foldmethod = "manual" }, -- window options
        },
      },
    },

    config = function()
      require("kulala").setup()
      vim.keymap.set("n", "<localleader>s", function()
        require("kulala").run()
      end, { desc = "Send request", buffer = true })
    end,
  },
}
