-- themes/extract.lua
--
-- Reads the active nvim colorscheme by querying highlight groups via
-- nvim_get_hl(0, { name, link = false }) and prints one tab-separated
-- (role, hex, source) triple per line. Output is consumed by the
-- `theme extract` subcommand of .local/bin/theme.
--
-- Invocation pattern (the `theme` CLI handles the plugin path resolution):
--
--   nvim --headless -u NONE \
--     --cmd "set runtimepath+=<plugin-dir>" \
--     --cmd "colorscheme <scheme>" \
--     --cmd "source <this-file>" \
--     -c "qa!"
--
-- See docs/superpowers/specs/2026-05-30-theming-design.md for the
-- canonical semantic vocabulary and fallback rationale.

local roles = {
  { name = "background",           chains = {{"Normal", "bg"}} },
  { name = "foreground",           chains = {{"Normal", "fg"}} },
  { name = "cursor",               chains = {{"Cursor", "bg"}, {"Normal", "fg"}} },
  { name = "selection_background", chains = {{"Visual", "bg"}} },
  { name = "selection_foreground", chains = {{"Visual", "fg"}, {"Normal", "fg"}} },

  { name = "comment",   chains = {{"@comment", "fg"}, {"Comment", "fg"}} },
  { name = "keyword",   chains = {{"@keyword", "fg"}, {"Keyword", "fg"}, {"Statement", "fg"}} },
  { name = "string",    chains = {{"@string", "fg"}, {"String", "fg"}} },
  { name = "function",  chains = {{"@function", "fg"}, {"Function", "fg"}} },
  { name = "type",      chains = {{"@type", "fg"}, {"Type", "fg"}} },
  { name = "number",    chains = {{"@number", "fg"}, {"Number", "fg"}, {"Constant", "fg"}} },
  { name = "variable",  chains = {{"@variable", "fg"}, {"Identifier", "fg"}} },
  { name = "constant",  chains = {{"@constant", "fg"}, {"Constant", "fg"}} },
  { name = "operator",  chains = {{"@operator", "fg"}, {"Operator", "fg"}} },
  { name = "property",  chains = {{"@property", "fg"}, {"@field", "fg"}, {"Identifier", "fg"}} },
  { name = "parameter", chains = {{"@parameter", "fg"}, {"@variable.parameter", "fg"}, {"Identifier", "fg"}} },

  { name = "error",   chains = {{"DiagnosticError", "fg"}, {"ErrorMsg", "fg"}} },
  { name = "warning", chains = {{"DiagnosticWarn", "fg"}, {"WarningMsg", "fg"}} },
  { name = "info",    chains = {{"DiagnosticInfo", "fg"}} },
  { name = "hint",    chains = {{"DiagnosticHint", "fg"}} },
}

local function resolve(chain)
  for _, c in ipairs(chain) do
    local ok, hl = pcall(vim.api.nvim_get_hl, 0, { name = c[1], link = false })
    if ok and hl and hl[c[2]] then
      return string.format("#%06X", hl[c[2]]), c[1] .. "." .. c[2]
    end
  end
  return nil, nil
end

local out = {}
table.insert(out, string.format("%-22s  %-9s  %s", "role", "hex", "source"))
table.insert(out, string.rep("-", 60))
for _, role in ipairs(roles) do
  local hex, source = resolve(role.chains)
  table.insert(out, string.format("%-22s  %-9s  %s", role.name, hex or "MISSING", source or "—"))
end

io.stdout:write(table.concat(out, "\n") .. "\n")
