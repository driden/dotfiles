local check_version = function()
  local verstr = tostring(vim.version())
  if not vim.version.ge then
    vim.health.error(string.format("Neovim out of date: '%s'. Upgrade to latest stable or nightly", verstr))
    return
  end

  if vim.version.ge(vim.version(), "0.10-dev") then
    vim.health.ok(string.format("Neovim version is: '%s'", verstr))
  else
    vim.health.error(string.format("Neovim out of date: '%s'. Upgrade to latest stable or nightly", verstr))
  end
end

local check_external_reqs = function()
  -- Basic utils: `git`, `make`, `unzip`
  for _, exe in ipairs({ "jq", "git", "make", "unzip", "rg", "mise" }) do
    local is_executable = vim.fn.executable(exe) == 1
    if is_executable then
      vim.health.ok(string.format("Found executable: '%s'", exe))
    else
      vim.health.warn(string.format("Could not find executable: '%s'", exe))
    end
  end

  return true
end

local check_lsp_requirements = function()
  local mise = require("driden.mise")
  local java_home = mise.get_mise_install_path_sync("java@corretto-21")

  if java_home then
    vim.health.ok(string.format("Found Java 21 for Kotlin LSP: '%s'", java_home))
  else
    vim.health.warn("Could not find Java 21 via mise - Kotlin language server will be disabled")
    vim.health.info("Install with: mise install java@corretto-21")
  end
end

return {
  check = function()
    vim.health.start("driden")
    vim.health.info("NOTE: Not every warning is a 'must-fix' in `:checkhealth` ")

    local uv = vim.uv or vim.loop
    vim.health.info("System Information: " .. vim.inspect(uv.os_uname()))

    check_version()
    check_external_reqs()
    check_lsp_requirements()
  end,
}
