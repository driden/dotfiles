local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

local keymap = vim.api.nvim_set_keymap

keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Buffers
keymap("n", "<leader>bk", ":bdelete<CR>", opts)
keymap("n", "<leader>bK", ":bdelete!<CR>", opts)

-- Windows
keymap("n", "<leader>wq", ":wincmd q<CR>", opts)
keymap("n", "<leader>wc", ":wincmd c<CR>", opts)

-- Finding Files
keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

keymap("n", "<C-p>", ":GFiles<CR>", opts)
keymap("n", "<Leader>ff", ":Files<CR>", opts)

-- Resize with arrows
keymap("n", "<Up>", ":resize +2<CR>", opts)
keymap("n", "<Down>", ":resize -2<CR>", opts)
keymap("n", "<Left>", ":vertical resize +3<CR>", opts)
keymap("n", "<Right>", ":vertical resize -3<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)
keymap("x", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Terminal --
-- Better terminal navigation
keymap("t", "<C-[>", "<C-\\><C-n>", term_opts)
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
keymap("t", "<leader>tc", "<C-><C-n>:q<CR>", term_opts)

-- Paste from OS
keymap("n", "<leader>p", "+p<CR>", term_opts)
keymap("n", "<leader>y", "+y<CR>", term_opts)

-- Tab
keymap("n", "<leader>tn", ":tabNext<CR>", opts)
keymap("n", "<leader>tp", ":tabprevious<CR>", opts)
keymap("n", "<leader>tc", ":tabclose<CR>", opts)

-- Git
keymap("n", "<leader>gj", ":diffget //3<CR>", opts) -- right
keymap("n", "<leader>gf", ":diffget //2<CR>", opts) -- left

keymap("n", "<leader>gs", ":G<CR>", opts)
keymap("n", "<leader>gc", ":GCheckout<CR>", opts)
keymap("n", "<leader>gg", ":Gvdiffsplit!<CR>", opts)
keymap("n", "<leader>gP", ":Git push<CR>", opts)
keymap("n", "<leader>gp", ":Git pull<CR>", opts)

-- Quick Fix
keymap("n", "<leader>qq", ":copen<CR>", opts) -- Open the quickfix window
keymap("n", "<leader>qc", ":ccl<CR>", opts) --  Open it if there are "errors", close it otherwise
keymap("n", "<leader>qe", ":cw<CR>", opts) --  Go to the next error in the window
keymap("n", "<leader>qn", ":cn<CR>", opts) -- Go to the previous error in the window
keymap("n", "<leader>qp", ":cp<CR>", opts) -- Go to the first error in the next file
keymap("n", "<leader>qf", ":cnf<CR>", opts) --Go to error under cursor (if cursor is in quickfix window)
keymap("n", "<leader>q.", ":.cc<CR>", opts)

-- Misc
keymap("n", "<leader>tm", "<cmd>lua require\"options\".toggle_meta_chars()<CR>", opts)

keymap("n", "<leader>pc", ":e ~/.config/nvim/init.vim<CR>", opts)
keymap("n", "<leader>rr", ":source ~/.config/nvim/init.vim<CR>", opts)
