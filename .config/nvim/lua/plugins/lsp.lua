return {
  -- Main LSP Configuration
  "neovim/nvim-lspconfig",
  dependencies = {
    -- Automatically install LSPs and related tools to stdpath for Neovim
    { "williamboman/mason.nvim", config = true }, -- NOTE: Must be loaded before dependants
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",

    { "j-hui/fidget.nvim", opts = {} },

    -- Allows extra capabilities provided by nvim-cmp
    "hrsh7th/cmp-nvim-lsp",
  },
  config = function()
    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
      callback = function(event)
        vim.keymap.set("n", "gk", vim.lsp.buf.signature_help, { buffer = true, silent = true, desc = "" })
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, { buffer = true, silent = true, desc = "" })
        vim.keymap.set("n", "gh", vim.lsp.buf.hover, { buffer = true, silent = true, desc = "[G]o [h]over" })
        vim.keymap.set("n", "ga", vim.lsp.buf.code_action, { buffer = true, silent = true, desc = "[G]o [a]ctions" })
        vim.keymap.set(
          "n",
          "gD",
          require("telescope.builtin").lsp_type_definitions,
          { buffer = true, silent = true, desc = "[G]o Type [D]efinition" }
        )
        vim.keymap.set(
          "n",
          "gd",
          require("telescope.builtin").lsp_definitions,
          { buffer = true, silent = true, desc = "[G]oto [d]efinition" }
        )
        vim.keymap.set(
          "n",
          "gi",
          require("telescope.builtin").lsp_implementations,
          { buffer = true, silent = true, desc = "[G]o [i]mplementation" }
        )
        vim.keymap.set(
          "n",
          "gr",
          require("telescope.builtin").lsp_references,
          { buffer = true, silent = true, desc = "[G]o [r]references" }
        )
        vim.keymap.set("n", "ge", vim.diagnostic.setqflist, { buffer = true, silent = true, desc = "[G]o [e]rrors" })
        vim.keymap.set(
          "n",
          "gs",
          require("telescope.builtin").lsp_document_symbols,
          { buffer = true, silent = true, desc = "[G]o [S]ymbols" }
        )
        vim.keymap.set(
          "n",
          "gE",
          vim.lsp.buf.declaration,
          { buffer = true, silent = true, desc = "[G]o d[E]claration" }
        )

        vim.keymap.set(
          "n",
          "<leader>ws",
          require("telescope.builtin").lsp_dynamic_workspace_symbols,
          { buffer = true, silent = true, desc = "[W]orkspace [S]ymbols" }
        )

        -- The following two autocommands are used to highlight references of the
        -- word under your cursor when your cursor rests there for a little while.
        --    See `:help CursorHold` for information about when this is executed
        --
        -- When you move your cursor, the highlights will be cleared (the second autocommand).
        local client = vim.lsp.get_client_by_id(event.data.client_id)
        if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
          local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
          vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
            buffer = event.buf,
            group = highlight_augroup,
            callback = vim.lsp.buf.document_highlight,
          })

          vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
            buffer = event.buf,
            group = highlight_augroup,
            callback = vim.lsp.buf.clear_references,
          })

          vim.api.nvim_create_autocmd("LspDetach", {
            group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
            callback = function(event2)
              vim.lsp.buf.clear_references()
              vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event2.buf })
            end,
          })
        end

        -- The following code creates a keymap to toggle inlay hints in your
        -- code, if the language server you are using supports them
        --
        -- This may be unwanted, since they displace some of your code
        if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
          vim.keymap.set("n", "<leader>th", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
          end, { desc = "[T]oggle Inlay [H]ints", silent = true })
        end
      end,
    })

    -- LSP servers and clients are able to communicate to each other what features they support.
    --  By default, Neovim doesn't support everything that is in the LSP specification.
    --  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
    --  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

    -- Enable the following language servers
    --  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
    --
    --  Add any additional override configuration in the following tables. Available keys are:
    --  - cmd (table): Override the default command used to start the server
    --  - filetypes (table): Override the default list of associated filetypes for the server
    --  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
    --  - settings (table): Override the default settings passed when initializing the server.
    --        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/
    local servers = {
      clangd = {},
      gopls = {},
      -- pyright = {},
      -- rust_analyzer = {},
      -- ... etc. See `:help lspconfig-all` for a list of all the pre-configured LSPs
      --
      -- Some languages (like typescript) have entire language plugins that can be useful:
      --    https://github.com/pmizio/typescript-tools.nvim
      --
      -- But for many setups, the LSP (`tsserver`) will work just fine
      -- tsserver = {},
      --,
      bashls = {},

      lua_ls = {
        -- cmd = {...},
        -- filetypes = { ...},
        -- capabilities = {},
        settings = {
          Lua = {
            completion = {
              callSnippet = "Replace",
            },
            -- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
            -- diagnostics = { disable = { 'missing-fields' } },
          },
        },
      },
      ts_ls = {},
    }

    -- Ensure the servers and tools above are installed
    --  To check the current status of installed tools and/or manually install
    --  other tools, you can run
    --    :Mason
    --
    --  You can press `g?` for help in this menu.
    require("mason").setup()

    -- You can add other tools here that you want Mason to install
    -- for you, so that they are available from within Neovim.
    local ensure_installed = vim.tbl_keys(servers or {})
    vim.list_extend(ensure_installed, {
      "stylua", -- Used to format Lua code
    })
    require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

    require("mason-lspconfig").setup({
      -- :h mason-lspconfig.setup_handlers()
      handlers = {
        function(server_name)
          local server = servers[server_name] or {}
          -- This handles overriding only values explicitly passed
          -- by the server configuration above. Useful when disabling
          -- certain features of an LSP (for example, turning off formatting for tsserver)
          server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
          require("lspconfig")[server_name].setup(server)
        end,
        csharp_ls = function()
          local csharp = require("lspconfig").csharp_ls.document_config.default_config
          csharp.cmd = { "csharpls" }
          require("lspconfig").csharp_ls.setup(csharp)
        end,
        jdtls = function()
          require("lspconfig").jdtls.setup({
            on_attach = function()
              local bemol_dir = vim.fs.find({ ".bemol" }, { upward = true, type = "directory" })[1]
              local ws_folders_lsp = {}
              if bemol_dir then
                local file = io.open(bemol_dir .. "/ws_root_folders", "r")
                if file then
                  for line in file:lines() do
                    table.insert(ws_folders_lsp, line)
                  end
                  file:close()
                end
              end
              for _, line in ipairs(ws_folders_lsp) do
                vim.lsp.buf.add_workspace_folder(line)
              end
            end,
            -- -XX:+UseParallelGC
            -- -XX:GCTimeRatio=4
            -- -XX:AdaptiveSizePolicyWeight=90
            -- -Dsun.zip.disableMemoryMapping=true
            -- -Xmx2G
            -- -Xms100m
            cmd = {
              "jdtls",
              "--jvm-arg=-javaagent:"
                .. require("mason-registry").get_package("jdtls"):get_install_path()
                .. "/lombok.jar",
            },
            format = {
              enabled = false,
              tabSize = 4,
            },
          })
        end,
      },
    })
  end,
}
