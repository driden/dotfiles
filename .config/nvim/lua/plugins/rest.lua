return {
  {
    "mistweaverco/kulala.nvim",
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
      local kulala = require("kulala")
      kulala.setup()

      local function set_request_keymaps(buffer)
        local opts = { buffer = buffer, silent = true }

        vim.keymap.set("n", "<localleader>s", kulala.run, vim.tbl_extend("force", opts, { desc = "Send request" }))
        vim.keymap.set(
          "n",
          "<localleader>a",
          kulala.run_all,
          vim.tbl_extend("force", opts, { desc = "Send all requests" })
        )
        vim.keymap.set(
          "n",
          "<localleader>b",
          kulala.scratchpad,
          vim.tbl_extend("force", opts, { desc = "Open scratchpad" })
        )
      end

      set_request_keymaps(0)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "http", "rest" },
        callback = function(event)
          set_request_keymaps(event.buf)
        end,
      })
    end,
  },
}
