require("neorg").setup({
	load = {
		["core.defaults"] = {},
		["core.concealer"] = {},
		["core.dirman"] = {
			config = {
				workspaces = {
					work = "~/Documents/Notes",
					notes = "~/notes",
				},
				default_workspace = "notes",
			},
		},
	},
})
