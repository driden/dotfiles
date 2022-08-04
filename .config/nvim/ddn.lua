local M = {}

function M.map(f, tbl)
  local t = {}
  for k, v in pairs(tbl) do
    t[k] = f(v)
  end
  return t
end

return M
