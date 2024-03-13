local formatting = require("null-ls").builtins.formatting
local eslint_d = {
  actions = require("none-ls.code_actions.eslint_d"),
  diagnostics = require("none-ls.diagnostics.eslint_d"),
  formatting = require("none-ls.formatting.eslint_d"),
}

local with_root_file = function(files)
  return function(utils)
    return utils.root_has_file(files)
  end
end

local stylua_root_files = { "stylua.toml", ".stylua.toml" }
local eslint_root_files = { ".eslintrc", ".eslintrc.js", ".eslintrc.cjs", ".eslintrc.json" }
local prettier_root_files = { ".prettierrc", ".prettierrc.js", ".prettierrc.json" }

require("null-ls").setup({
  debug = false, -- :NullLsLog && :NullLsInfo
  root_dir = require("null-ls.utils").root_pattern(".null-ls-root", "Makefile", ".git", "package.json"),
  sources = {
    eslint_d.actions.with({
      condition = with_root_file(eslint_root_files),
    }),
    eslint_d.diagnostics.with({
      condition = with_root_file(eslint_root_files),
    }),
    eslint_d.formatting.with({
      condition = function(utils)
        local has_eslint = with_root_file(eslint_root_files)(utils)
        local has_prettier = with_root_file(prettier_root_files)(utils)
        return has_eslint and not has_prettier
      end,
    }),
    formatting.prettier.with({
      condition = with_root_file(prettier_root_files),
    }),
    formatting.shfmt,
    formatting.stylua.with({ condition = with_root_file(stylua_root_files) }),
    --formatting.google_java_format
  },
  on_attach = require("lsp.keymaps").on_attach,
})
