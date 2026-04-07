local M = {}

function M.setup(capabilities)
  ---@brief
  ---
  --- SuperHTML - HTML Validator, Formatter, LSP, and Templating Language Library
  --- Download from: https://github.com/kristoff-it/superhtml/releases
  ---
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  vim.lsp.config("html", {
    cmd = { "superhtml", "lsp" },
    filetypes = { "html", "shtml", "htm" },
    root_markers = { ".git" },
    init_options = {
      syntax_only = true,
    },
    settings = {},
  })

  vim.lsp.enable("html")
end

return M
