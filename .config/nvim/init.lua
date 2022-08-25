--[[
--TODOS:
--      * Telescope
--      * lua themes
--      * lualine
--      * Kotlin lsp
--      * DAP
--      * Code lenses
--]]
require("plugins.plugin_manager")
require("plugins.autocomplete").setup()
require("plugins.file_tree").setup()
require("plugins.lsp")
require("plugins.toggleterm").setup()
require("plugins.treesitter").setup()
require("options").setup()
require("themes").load_theme()
require("keymaps")
require("user_commands").load_commands()
require("utils.general")
require("highlight")
