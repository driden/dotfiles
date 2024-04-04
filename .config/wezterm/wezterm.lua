local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.automatically_reload_config = true
config.window_background_opacity = 0.7

return config
