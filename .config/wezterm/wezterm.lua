local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = "Cyberdyne"
-- config.color_scheme = "cyberpunk"
-- config.color_scheme = "CutiePro"
config.font_size = 14

config.font = wezterm.font_with_fallback({
	{ family = "Comic Code", weight = "Medium", stretch = "Normal", style = "Normal" },
	-- { family = "Comic Code", weight = "Regular", stretch = "Normal", style = "Normal" },
	-- { family = "Comic Code", weight = "Regular", stretch = "Normal", style = "Italic" },
	-- { family = "Comic Code", weight = "DemiBold", stretch = "Normal", style = "Normal" },
	-- { family = "Comic Code", weight = "Bold", stretch = "Normal", style = "Normal" },
	{ family = "Comic Code" },
	{ family = "Comic Code", weight = "Medium" },
	{ family = "Iosevka" },
	{ family = "Hack" },
	{ family = "Monaco" },
	{ family = "Hack Nerd Font", weight = "Regular" },
	{ family = "Comic Mono", weight = "Regular" },
	{ family = "JetBrains Mono", weight = "Medium" },
	{ family = "CodeNewRoman NF", weight = "Medium" },
})

config.tab_max_width = 35
config.color_scheme = "Catppuccin Mocha"
config.automatically_reload_config = true
config.window_background_opacity = 0.9
config.hide_tab_bar_if_only_one_tab = true

-- For readline to work on macos, will probably need to query for the os beforehand
config.send_composed_key_when_right_alt_is_pressed = false

config.keys = {
	-- CTRL-SHIFT-l activates the debug overlay
	{ key = "L", mods = "CTRL", action = wezterm.action.ShowDebugOverlay },
	{
		key = "p",
		mods = "CTRL|SHIFT",
		action = wezterm.action.SplitPane({
			direction = "Right",
			size = { Percent = 50 },
		}),
	},
	{
		key = "s",
		mods = "CTRL|SHIFT",
		action = wezterm.action.SplitPane({
			direction = "Down",
			size = { Percent = 50 },
		}),
	},
	{
		key = "r",
		mods = "CTRL|SHIFT",
		action = wezterm.action.PromptInputLine({
			description = "Tab",
			action = wezterm.action_callback(function(window, pane, line)
				if window then
					window:active_tab():set_title(line)
				end
			end),
		}),
	},
	{
		key = "w",
		mods = "CTRL|SHIFT",
		action = wezterm.action.CloseCurrentPane({ confirm = false }),
	},

	{
		key = "j",
		mods = "CTRL|SHIFT|CMD",
		action = wezterm.action.ActivatePaneDirection("Down"),
	},
	{
		key = "k",
		mods = "CTRL|SHIFT|CMD",
		action = wezterm.action.ActivatePaneDirection("Up"),
	},
	{
		key = "h",
		mods = "CTRL|SHIFT|CMD",
		action = wezterm.action.ActivatePaneDirection("Left"),
	},
	{
		key = "l",
		mods = "CTRL|SHIFT|CMD",
		action = wezterm.action.ActivatePaneDirection("Right"),
	},
	{
		key = "h",
		mods = "CTRL|SHIFT",
		action = wezterm.action.ActivateTabRelative(-1),
	},
	{
		key = "l",
		mods = "CTRL|SHIFT",
		action = wezterm.action.ActivateTabRelative(1),
	},
	{
		key = "h",
		mods = "SUPER",
		action = wezterm.action.MoveTabRelative(1),
	},
	{
		key = "l",
		mods = "SUPER",
		action = wezterm.action.MoveTabRelative(0),
	},
}

config.window_decorations = "INTEGRATED_BUTTONS|TITLE|RESIZE"
-- config.window_decorations = "TITLE|RESIZE"
config.tab_bar_at_bottom = true
config.enable_tab_bar = true
config.use_fancy_tab_bar = false

config.window_padding = {
	left = 2,
	right = 2,
	top = 0,
	bottom = 0,
}

-- https://github.com/danielcopper/dotfiles/blob/6e8318354c62ab7bb2b2708afc0cf8ee54f2dc22/.wezterm.lua
-- Functions
local get_last_folder_segment = function(cwd)
	if cwd == "" then
		return "N/A" -- or some default value you prefer
	end

	-- Strip off 'file:///' if present
	local pathStripped = cwd:match("^file:///(.+)") or cwd
	-- Normalize backslashes to slashes for Windows paths
	pathStripped = pathStripped:gsub("\\", "/")
	-- Split the path by '/'
	local path = {}
	for segment in string.gmatch(pathStripped, "[^/]+") do
		table.insert(path, segment)
	end
	return path[#path] -- returns the last segment
end

local function get_current_working_dir(tab)
	local current_dir = tab.active_pane.current_working_dir.file_path or ""
	wezterm.log_info("cwd: " .. current_dir)
	return get_last_folder_segment(current_dir)
end

local process_icons = {
	["bash"] = wezterm.nerdfonts.cod_terminal_bash,
	["btm"] = wezterm.nerdfonts.mdi_chart_donut_variant,
	["cargo"] = wezterm.nerdfonts.dev_rust,
	["curl"] = wezterm.nerdfonts.mdi_flattr,
	["docker"] = wezterm.nerdfonts.linux_docker,
	["docker-compose"] = wezterm.nerdfonts.linux_docker,
	["dotnet"] = wezterm.nerdfonts.md_language_csharp,
	["gh"] = wezterm.nerdfonts.dev_github_badge,
	["git"] = wezterm.nerdfonts.dev_git,
	["go"] = wezterm.nerdfonts.seti_go,
	["htop"] = wezterm.nerdfonts.mdi_chart_donut_variant,
	["kubectl"] = wezterm.nerdfonts.linux_docker,
	["kuberlr"] = wezterm.nerdfonts.linux_docker,
	["lazydocker"] = wezterm.nerdfonts.linux_docker,
	["lua"] = wezterm.nerdfonts.seti_lua,
	["make"] = wezterm.nerdfonts.seti_makefile,
	["node"] = wezterm.nerdfonts.dev_nodejs_small,
	["nvim"] = wezterm.nerdfonts.custom_vim,
	["psql"] = wezterm.nerdfonts.dev_postgresql,
	["pwsh"] = wezterm.nerdfonts.seti_powershell,
	["ruby"] = wezterm.nerdfonts.cod_ruby,
	["stern"] = wezterm.nerdfonts.linux_docker,
	["sudo"] = wezterm.nerdfonts.fa_hashtag,
	["vim"] = wezterm.nerdfonts.dev_vim,
	["wget"] = wezterm.nerdfonts.mdi_arrow_down_box,
	["zsh"] = wezterm.nerdfonts.dev_terminal,
}

local function get_process(tab)
	local process_name = tab.active_pane.foreground_process_name:match("([^/\\]+)%.exe$")
		or tab.active_pane.foreground_process_name:match("([^/\\]+)$")

	-- local icon = process_icons[process_name] or string.format('[%s]', process_name)
	local icon = process_icons[process_name] or wezterm.nerdfonts.seti_checkbox_unchecked

	return icon
end

wezterm.on("format-tab-title", function(tab, tabs, panes, conf, hover, max_width)
	local is_zoomed = false
	local cwd = get_current_working_dir(tab)
	local process = get_process(tab)
	local zoom_icon = is_zoomed and wezterm.nerdfonts.cod_zoom_in or ""

	local title = string.format(" %s ~ %s %s ", process, cwd, zoom_icon) -- Add placeholder for zoom_icon
	return title
end)

config.window_close_confirmation = "NeverPrompt"

return config
