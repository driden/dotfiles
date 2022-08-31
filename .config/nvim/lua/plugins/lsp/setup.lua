-- If you are looking for java, then its at ../../ftplugin/

--local function get_lua_opts()
--  return {
--    settings = {
--      Lua = {
--        runtime = {
--          version = "LuaJIT",
--          path = vim.split(package.path, ";"),
--        },
--        diagnostics = { globals = { "vim", "hs" } },
--        workspace = {
--          library = vim.api.nvim_get_runtime_file("", true),
--          checkThirdParty = false,
--        },
--        telemetry = { enable = false },
--      },
--    },
--  }
--end

--local function setup_server(server)
--  local opts = {}
--  if server.name == "sumneko_lua" then
--    opts = vim.tbl_deep_extend("force", get_lua_opts(), opts)
--  end
--  server:setup(opts)
--end

vim.o.completeopt = "menu,menuone,noselect"
require("nvim-lsp-installer").setup {
  ensure_installed = { "eslint", "tsserver", "sumneko_lua" }
}

local lspconfig = require("lspconfig")
lspconfig.gopls.setup {}
lspconfig.tsserver.setup {}
lspconfig.eslint.setup {}
lspconfig.sumneko_lua.setup {
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
        path = vim.split(package.path, ";"),
      },
      diagnostics = { globals = { "vim", "hs" } },
      workspace = {
        [vim.fn.expand "$VIMRUNTIME/lua"] = true,
        [vim.fn.stdpath "config" .. "/lua"] = true,
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = { enable = false },
    },
  },
}

-- This setup() function will take the provided server configuration and decorate it with the necessary properties
-- before passing it onwards to lspconfig.
-- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

-- nullls
local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics

require("null-ls").setup {
  debug = false, -- :NullLsLog && :NullLsInfo
  sources = {
    formatting.eslint,
    formatting.shfmt,
    formatting.stylua,
    formatting.eslint,
    diagnostics.eslint,
  },
  on_attach = function(_, bufnr)
    local opts = { noremap = true, silent = true }
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gk", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gh", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "ga", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
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
}
