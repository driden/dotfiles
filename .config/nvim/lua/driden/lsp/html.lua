local M = {}

function M.setup(capabilities)
  ---@brief
  ---
  --- SuperHTML - HTML Validator, Formatter, LSP, and Templating Language Library
  --- Download from: https://github.com/kristoff-it/superhtml/releases
  ---
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  -- Triggering completion on space is horrible dx
  if capabilities.textDocument.completion.completionItem.triggerCharacters then
    capabilities.textDocument.completion.completionItem.triggerCharacters = vim.tbl_filter(function(c)
      return c ~= " "
    end, capabilities.textDocument.completion.completionItem.triggerCharacters)
  end

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
