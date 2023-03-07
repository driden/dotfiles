-- If you are looking for java, then its at ../../ftplugin/
local formatting = require("null-ls").builtins.formatting
local diagnostics = require("null-ls").builtins.diagnostics
local code_actions = require("null-ls").builtins.code_actions

local with_root_file = function(...)
  local files = { ... }
  return function(utils)
    return utils.root_has_file(files)
  end
end

-- https://github.com/0x221A/dotfiles/blob/2a1cc44bf39268222377d8c6244c9535fd493b83/packages/nvim/nvim/lua/rootblack45/configs/lsp/null-ls.lua#L18
local project_node_bin = 'node_modules/.bin'
local local_eslint = project_node_bin .. '/eslint'

require("null-ls").setup({
    debug = false, -- :NullLsLog && :NullLsInfo
    sources = {
        code_actions.eslint_d,
        diagnostics.eslint_d.with({
            condition = with_root_file(local_eslint)
        }),
        formatting.eslint_d.with({
            condition = with_root_file(local_eslint)
        }),
        formatting.shfmt,
        formatting.stylua,
        --formatting.google_java_format
    },
    on_attach = require("lsp.keymaps").on_attach,
})
