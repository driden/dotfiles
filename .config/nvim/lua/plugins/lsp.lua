local function on_attach_evt(client, bufnr)
  vim.keymap.set("n", "gk", vim.lsp.buf.signature_help, { buffer = true, silent = true, desc = "" })
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, { buffer = true, silent = true, desc = "" })
  vim.keymap.set("n", "gh", vim.lsp.buf.hover, { buffer = true, silent = true, desc = "[G]o [h]over" })
  vim.keymap.set("n", "ga", vim.lsp.buf.code_action, { buffer = true, silent = true, desc = "[G]o [a]ctions" })
  vim.keymap.set(
    "n",
    "gD",
    require("telescope.builtin").lsp_type_definitions,
    { buffer = bufnr, silent = true, desc = "[G]o Type [D]efinition" }
  )
  vim.keymap.set(
    "n",
    "gd",
    require("telescope.builtin").lsp_definitions,
    { buffer = bufnr, silent = true, desc = "[G]oto [d]efinition" }
  )
  vim.keymap.set(
    "n",
    "gi",
    require("telescope.builtin").lsp_implementations,
    { buffer = bufnr, silent = true, desc = "[G]o [i]mplementation" }
  )
  vim.keymap.set(
    "n",
    "gr",
    require("telescope.builtin").lsp_references,
    { buffer = bufnr, silent = true, desc = "[G]o [r]references" }
  )
  vim.keymap.set("n", "ge", vim.diagnostic.setqflist, { buffer = bufnr, silent = true, desc = "[G]o [e]rrors" })
  vim.keymap.set(
    "n",
    "gs",
    require("telescope.builtin").lsp_document_symbols,
    { buffer = bufnr, silent = true, desc = "[G]o [S]ymbols" }
  )
  vim.keymap.set("n", "gE", vim.lsp.buf.declaration, { buffer = bufnr, silent = true, desc = "[G]o d[E]claration" })

  vim.keymap.set(
    "n",
    "<leader>ws",
    require("telescope.builtin").lsp_dynamic_workspace_symbols,
    { buffer = bufnr, silent = true, desc = "[W]orkspace [S]ymbols" }
  )

  -- The following two autocommands are used to highlight references of the
  -- word under your cursor when your cursor rests there for a little while.
  --    See `:help CursorHold` for information about when this is executed
  --
  -- When you move your cursor, the highlights will be cleared (the second autocommand).
  if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
    local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      buffer = bufnr,
      group = highlight_augroup,
      callback = vim.lsp.buf.document_highlight,
    })

    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
      buffer = bufnr,
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
  if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, bufnr) then
    vim.keymap.set("n", "<leader>th", function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr }))
    end, { desc = "[T]oggle Inlay [H]ints", silent = true })
  end
end

local function on_attach(event)
  local client = vim.lsp.get_client_by_id(event.data.client_id)
  return on_attach_evt(client, event.buf)
end

return {
  {
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
        callback = on_attach,
      })

      -- Diagnostic Config
      -- See :help vim.diagnostic.Opts
      vim.diagnostic.config({
        severity_sort = true,
        float = { border = "rounded", source = "if_many" },
        underline = { severity = vim.diagnostic.severity.ERROR },
        signs = vim.g.have_nerd_font and {
          text = {
            [vim.diagnostic.severity.ERROR] = "󰅚 ",
            [vim.diagnostic.severity.WARN] = "󰀪 ",
            [vim.diagnostic.severity.INFO] = "󰋽 ",
            [vim.diagnostic.severity.HINT] = "󰌶 ",
          },
        } or {},
        virtual_text = {
          source = "if_many",
          spacing = 2,
          format = function(diagnostic)
            local diagnostic_message = {
              [vim.diagnostic.severity.ERROR] = diagnostic.message,
              [vim.diagnostic.severity.WARN] = diagnostic.message,
              [vim.diagnostic.severity.INFO] = diagnostic.message,
              [vim.diagnostic.severity.HINT] = diagnostic.message,
            }
            return diagnostic_message[diagnostic.severity]
          end,
        },
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
      -- local servers = {
      --   clangd = {},
      --   gopls = {},
      --   -- pyright = {},
      --   -- rust_analyzer = {},
      --   -- ... etc. See `:help lspconfig-all` for a list of all the pre-configured LSPs
      --   --
      --   -- Some languages (like typescript) have entire language plugins that can be useful:
      --   --    https://github.com/pmizio/typescript-tools.nvim
      --   --
      --   -- But for many setups, the LSP (`tsserver`) will work just fine
      --   -- tsserver = {},
      --   --,
      --   bashls = {},
      --   ts_ls = {},
      -- }

      -- You can add other tools here that you want Mason to install
      -- for you, so that they are available from within Neovim.
      local ensure_installed = vim.tbl_keys(servers or {})
      vim.list_extend(ensure_installed, {
        "stylua", -- Used to format Lua code
        "kotlin-lsp", -- v0.252.17811
      })
      require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

      -- Ideally this won't be needed once I update nvim and it's plugins.
      -- require("mason-lspconfig").setup({
      --   -- :h mason-lspconfig.setup_handlers()
      --   handlers = {
      --     function(server_name)
      --       local server = servers[server_name] or {}
      --       -- This handles overriding only values explicitly passed
      --       -- by the server configuration above. Useful when disabling
      --       -- certain features of an LSP (for example, turning off formatting for tsserver)
      --       server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
      --       vim.print(server_name, server)
      --       require("lspconfig")[server_name].setup(server)
      --     end,
      --     csharp_ls = function()
      --       local csharp = require("lspconfig").csharp_ls.document_config.default_config
      --       csharp.cmd = { "csharpls" }
      --       require("lspconfig").csharp_ls.setup(csharp)
      --       vim.lsp.config("csharp_ls", csharp)
      --     end,
      --     -- jdtls = function()
      --     --   require("lspconfig").jdtls.setup({
      --     --     on_attach = function()
      --     --       local bemol_dir = vim.fs.find({ ".bemol" }, { upward = true, type = "directory" })[1]
      --     --       local ws_folders_lsp = {}
      --     --       if bemol_dir then
      --     --         local file = io.open(bemol_dir .. "/ws_root_folders", "r")
      --     --         if file then
      --     --           for line in file:lines() do
      --     --             table.insert(ws_folders_lsp, line)
      --     --           end
      --     --           file:close()
      --     --         end
      --     --       end
      --     --       for _, line in ipairs(ws_folders_lsp) do
      --     --         vim.lsp.buf.add_workspace_folder(line)
      --     --       end
      --     --     end,
      --     --     -- -XX:+UseParallelGC
      --     --     -- -XX:GCTimeRatio=4
      --     --     -- -XX:AdaptiveSizePolicyWeight=90
      --     --     -- -Dsun.zip.disableMemoryMapping=true
      --     --     -- -Xmx2G
      --     --     -- -Xms100m
      --     --     cmd = {
      --     --       "jdtls",
      --     --       "--jvm-arg=-javaagent:"
      --     --         .. require("mason-registry").get_package("jdtls"):get_install_path()
      --     --         .. "/lombok.jar",
      --     --     },
      --     --     format = {
      --     --       enabled = false,
      --     --       tabSize = 4,
      --     --     },
      --     --   })
      --     -- end,
      --   },
      -- })

      local kotlin_root_markers = {
        "settings.gradle", -- Gradle (multi-project)
        "settings.gradle.kts", -- Gradle (multi-project)
        "pom.xml", -- Maven
        "build.gradle", -- Gradle
        "build.gradle.kts", -- Gradle
      }

      -- This is the new way of setting lsp up, will need to look into why nvim-lspconfig is not working, might be an update thing
      -- vim.lsp.config("kotlin_lsp", {
      --   filetypes = { "kotlin" },
      --   cmd = { "kotlin-lsp", "--stdio" },
      --   root_markers = kotlin_root_markers,
      -- })
      --
      -- vim.lsp.enable("kotlin_lsp")

      -- Setup Kotlin language server (requires Java 21 from mise)
      local mise = require("driden.mise")
      local java_home = mise.get_mise_install_path_sync("java@corretto-21")
      
      if java_home then
        vim.lsp.config("kotlin_language_server", {
          filetypes = { "kotlin" },
          cmd = { "kotlin-language-server" },
          cmd_env = {
            JAVA_HOME = java_home,
          },
          root_markers = kotlin_root_markers,
          init_options = {
            -- Enables caching and use project root to store cache data.
            storagePath = vim.fs.root(vim.fn.expand("%:p:h"), kotlin_root_markers) --[[@as string]],
          },
        })

        vim.lsp.enable("kotlin_language_server")
      end

      -- Setup LSP cleanup on exit to prevent dangling processes
      vim.api.nvim_create_autocmd("VimLeavePre", {
        group = vim.api.nvim_create_augroup("lsp-cleanup", { clear = true }),
        callback = function()
          -- Stop all LSP clients gracefully before Neovim exits
          local clients = vim.lsp.get_clients()
          for _, client in ipairs(clients) do
            vim.lsp.stop_client(client.id, true) -- true = force stop
          end
        end,
      })

      local lua_root_markers1 = {
        ".emmyrc.json",
        ".luarc.json",
        ".luarc.jsonc",
      }
      local lua_root_markers2 = {
        ".luacheckrc",
        ".stylua.toml",
        "stylua.toml",
        "selene.toml",
        "selene.yml",
      }

      vim.lsp.config("lua_ls", {
        cmd = { "lua-language-server" },
        filetypes = { "lua" },
        root_markers = vim.fn.has("nvim-0.11.3") == 1 and { lua_root_markers1, lua_root_markers2, { ".git" } }
          or vim.list_extend(vim.list_extend(lua_root_markers1, lua_root_markers2), { ".git" }),
        settings = {
          Lua = {
            codeLens = { enable = true },
            hint = { enable = true, semicolon = "Disable" },
            completion = {
              callSnippet = "Replace",
              diagnostics = { disable = { "missing-fields" } },
            },
          },
        },
      })

      vim.lsp.enable("lua_ls")

      local ruffc = vim.deepcopy(capabilities)
      ruffc.general.positionEncodings = { "utf-16" }
      -- ruffc.offsetEncoding = "utf-16"
      vim.lsp.config("ruff", {
        capabilities = ruffc,
        cmd = { "ruff", "server" },
        filetypes = { "python" },
        root_markers = { "pyproject.toml", "ruff.toml", ".ruff.toml", ".git" },
        settings = {},
      })
      vim.lsp.enable("ruff")
    end,
  },
  {
    "scalameta/nvim-metals",
    ft = { "scala", "sbt", "java" },
    opts = function()
      local metals_config = require("metals").bare_config()
      metals_config.on_attach = on_attach_evt
      metals_config.useGlobalExecutable = true
      return metals_config
    end,
    config = function(self, metals_config)
      local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = self.ft,
        callback = function()
          require("metals").initialize_or_attach(metals_config)
        end,
        group = nvim_metals_group,
      })
    end,
  },

  require("mason-nvim-dap").setup(),
}
