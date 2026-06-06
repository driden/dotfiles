-- themes/extract.lua
--
-- Reads the active nvim colorscheme by querying highlight groups via
-- nvim_get_hl(0, { name, link = false }) and prints a TOML semantic
-- palette to stdout (consumed by the `theme extract` subcommand of
-- .local/bin/theme). Unresolved roles are reported to stderr.
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

-- Resolve all roles up front so we can write stderr and stdout separately.
local resolved = {}
for _, role in ipairs(roles) do
  local hex, source = resolve(role.chains)
  resolved[role.name] = { hex = hex or "#000000", source = source or "MISSING" }
end

-- stderr: only report unresolved roles
local missing_count = 0
for _, role in ipairs(roles) do
  local r = resolved[role.name]
  if r.source == "MISSING" then
    io.stderr:write("theme extract: unresolved role: " .. role.name .. "\n")
    missing_count = missing_count + 1
  end
end
if missing_count > 0 then
  io.stderr:write("theme extract: " .. missing_count .. " role(s) not resolved\n")
end

-- stdout: TOML palette
local chrome = { "background", "foreground", "cursor", "selection_background", "selection_foreground" }
local syntax = { "comment", "keyword", "string", "function", "type", "number", "variable", "constant", "operator", "property", "parameter" }
local diagnostics = { "error", "warning", "info", "hint" }

assert(#chrome + #syntax + #diagnostics == #roles,
  "group lists out of sync with roles table — update chrome/syntax/diagnostics")

local out = {}

table.insert(out, '[meta]')
table.insert(out, 'name = "PLACEHOLDER"')
table.insert(out, 'appearance = "dark"')
table.insert(out, '')
table.insert(out, '[palette]')

for _, name in ipairs(chrome) do
  table.insert(out, string.format("%-21s= %q", name .. " ", resolved[name].hex))
end

table.insert(out, '')

for _, name in ipairs(syntax) do
  table.insert(out, string.format("%-10s= %q", name .. " ", resolved[name].hex))
end

table.insert(out, '')

for _, name in ipairs(diagnostics) do
  table.insert(out, string.format("%-8s= %q", name .. " ", resolved[name].hex))
end

io.stdout:write(table.concat(out, "\n") .. "\n")
