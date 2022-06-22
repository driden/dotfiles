-- If you are looking for java, then its at ../../ftplugin/
local M = {}

local function get_lua_opts()
  return {
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT",
          path = vim.split(package.path, ";"),
        },
        diagnostics = { globals = { "vim", "hs" } },
        workspace = {
          library = vim.api.nvim_get_runtime_file("", true),
          checkThirdParty = false,
        },
        telemetry = { enable = false },
      },
    },
  }
end

local function setup_server(server)
  local opts = {}
  if server.name == "sumneko_lua" then
    opts = vim.tbl_deep_extend("force", get_lua_opts(), opts)
  end
  server:setup(opts)
end

function M.setup()
  vim.o.completeopt = "menu,menuone,noselect"
  local installer = require("nvim-lsp-installer")
  installer.on_server_ready(setup_server)
  -- This setup() function will take the provided server configuration and decorate it with the necessary properties
  -- before passing it onwards to lspconfig.
  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

  -- nullls
  local null_ls = require("null-ls")
  local formatting = null_ls.builtins.formatting
  null_ls.setup({
    sources = {
      formatting.clang_format,
      formatting.cmake_format,
      formatting.dart_format,
      formatting.gofmt,
      --formatting.prettier,
      formatting.shfmt,
      formatting.stylua.with({ indent_type = "Spaces" }),
      null_ls.builtins.completion.spell,
      null_ls.builtins.diagnostics.eslint,
    },
    on_attach = function(client, bufnr)
      if client.resolved_capabilities.document_formatting then
        vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()")
        if client.resolved_capabilities.document_highlight then
          -- TODO: Neovim has a new api for autocommands, check that out
          vim.api.nvim_exec(
            [[
          augroup document_highlight
            autocmd! * <buffer>
            autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
            autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
            augroup END
          ]] ,
            false
          )
        end
      end
      local opts = { noremap = true, silent = true }
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gk", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gh", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "ga", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
      vim.api.nvim_buf_set_keymap(
        bufnr,
        "n",
        "<space>wa",
        "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>",
        opts
      )
      vim.api.nvim_buf_set_keymap(
        bufnr,
        "n",
        "<space>wr",
        "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>",
        opts
      )
      vim.api.nvim_buf_set_keymap(
        bufnr,
        "n",
        "<space>wl",
        "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
        opts
      )
      vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
      vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>cf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    end,
  })
end

return M
