vim.api.nvim_buf_create_user_command(0, "Jq", "%!jq", {})
vim.api.nvim_buf_create_user_command(0, "JqSortKeys", "%!jq --sort-keys", {})
