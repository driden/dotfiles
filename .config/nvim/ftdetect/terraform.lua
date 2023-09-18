vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
	pattern = { "*.tf", "*.tfvars" },
	command = "setfiletype terraform",
})
