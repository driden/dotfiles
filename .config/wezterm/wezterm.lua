local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.automatically_reload_config = true
config.window_background_opacity = 0.7

config.keys = {
  -- CTRL-SHIFT-l activates the debug overlay
  { key = 'L', mods = 'CTRL', action = wezterm.action.ShowDebugOverlay },
  {
    key = 'v',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.SplitPane {
      direction = 'Right',
      size = { Percent = 50 },
    },
  },
  {
    key = 's',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.SplitPane {
      direction = 'Down',
      size = { Percent = 50 },
    },
  },
  {
    key = 'j',
    mods = 'CTRL|SHIFT|CMD',
    action = wezterm.action.ActivatePaneDirection 'Down',
  },
  {
    key = 'k',
    mods = 'CTRL|SHIFT|CMD',
    action = wezterm.action.ActivatePaneDirection 'Up',
  },
  {
    key = 'h',
    mods = 'CTRL|SHIFT|CMD',
    action = wezterm.action.ActivatePaneDirection 'Left',
  },
  {
    key = 'l',
    mods = 'CTRL|SHIFT|CMD',
    action = wezterm.action.ActivatePaneDirection 'Right',
  },
  {
    key = 'h',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.ActivateTabRelative(-1)
  },
  {
    key = 'l',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.ActivateTabRelative(1)
  }
}

return config
