-- 0 is current buffer
vim.api.nvim_buf_set_keymap(0, "n", "<leader>c", ":!lua %:t<CR>", { noremap = true, silent = true })
