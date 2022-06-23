require "plugins.plugin_manager".setup()
require "plugins.autocomplete".setup()
require "plugins.file_tree".setup()
require "plugins.lsp".setup()
require "plugins.toggleterm".setup()
require "plugins.treesitter".setup()
require "options".setup()
require "themes".load_theme()
require "keymaps"
require "user_commands".load_commands()

