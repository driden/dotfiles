" -------------------------------------------------------------------------
"    Plugins START
" -------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'nvim-lua/plenary.nvim'
" THEMES

" Plug 'drewtempelmeyer/palenight.vim'
" Plug 'joshdick/onedark.vim'
" Plug 'sainnhe/gruvbox-material'
Plug 'NLKNguyen/papercolor-theme'
Plug 'rktjmp/lush.nvim'
Plug 'metalelf0/jellybeans-nvim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround' 
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'akinsho/toggleterm.nvim'

" FileTree
Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'


"Treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} 

" Org
Plug 'nvim-orgmode/orgmode'
" LSP
" NO AGREGARRR Plug 'mfussenegger/nvim-jdtls'
Plug 'neovim/nvim-lspconfig'
Plug 'onsails/lspkind-nvim'
Plug 'williamboman/nvim-lsp-installer'
Plug 'jose-elias-alvarez/null-ls.nvim'

"" Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/cmp-nvim-lua'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
Plug 'hrsh7th/cmp-nvim-lsp-document-symbol'

" For vsnip users.
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

" For luasnip users.
" Plug 'L3MON4D3/LuaSnip'
" Plug 'saadparwaiz1/cmp_luasnip'

" For ultisnips users.
" Plug 'SirVer/ultisnips'
" Plug 'quangnguyen30192/cmp-nvim-ultisnips'

" For snippy users.
" Plug 'dcampos/nvim-snippy'
" Plug 'dcampos/cmp-snippy'

call plug#end()
" -------------------------------------------------------------------------
"    Plugins END
" -------------------------------------------------------------------------
" Plugins directives
let g:airline_theme='minimalist'

" -------------------------------------------------------------------------
" VIM GENERAL SETTINGS
" -------------------------------------------------------------------------

let mapleader=" "

colorscheme PaperColor 

command JqBuffer execute "%!jq"

" choose right
nmap <leader>gj :diffget //3<CR>
" choose left
nmap <leader>gf :diffget //2<CR>

nmap <leader>gs :G<CR>
nmap <leader>gc :GCheckout<CR>
nmap <leader>gg :Gvdiffsplit!<CR>
nmap <leader>gP :Git push<CR>
nmap <leader>gp :Git pull<CR>

nnoremap <C-p> :GFiles<CR>
nnoremap <Leader>pf :Files<CR>
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>pv :Explore<CR>

" Windows
nnoremap <leader>wq :wincmd q<CR>
nnoremap <leader>wc :wincmd c<CR>

nnoremap <leader>wj :wincmd j<CR>
nnoremap <leader>wk :wincmd k<CR>
nnoremap <leader>wh :wincmd h<CR>
nnoremap <leader>wl :wincmd l<CR>
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>

" vertical resize
noremap <leader>w] :vertical resize +10<CR>
noremap <leader>w[ :vertical resize -10<CR>

" horizontal resize
noremap <leader>w= :resize +10<CR>
noremap <leader>w- :resize -10<CR>

"<CR> Buffers
nnoremap <leader>bd :bd<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>

" Tabs
nnoremap <leader>tn :tabNext<CR>
nnoremap <leader>tp :tabprevious<CR>
nnoremap <leader>tc :tabclose<CR>
" Paste from OS
nnoremap <leader>p "+p<CR>
nnoremap <leader>y "+y<CR>

" Terminal
tnoremap <C-[> <C-\><C-n>
nnoremap <leader>tt :vs<CR>:term<CR>
nnoremap <leader>th :sp<CR>:term<CR>
nnoremap <leader>tc <C-\><C-n>:q<CR>

" Edit this file
"
nnoremap <leader>pc :e ~/.config/nvim/init.vim<CR>
" Source config
nnoremap <leader>rr :source ~/.config/nvim/init.vim<CR>

" QuickFix
" Open the quickfix window
nnoremap <leader>qq :copen<CR>
" Close it
nnoremap <leader>qc :ccl<CR>
" Open it if there are "errors", close it otherwise (some people prefer this)
nnoremap <leader>qe :cw<CR>
" Go to the next error in the window
nnoremap <leader>qn :cn<CR>
" Go to the previous error in the window
nnoremap <leader>qp :cp<CR>
" Go to the first error in the next file
nnoremap <leader>qf :cnf<CR>
" Go to error under cursor (if cursor is in quickfix window)
nnoremap <leader>q. :.cc<CR>
" Explorer 
nnoremap <leader>fv :20NnvimTreeToggle<CR>

" Dont copy replaced text in visual mode, you can past the same thing many
" times
xnoremap p "_dP
"
" Changing(or deleting) a word shouldnt alter the default register
nnoremap c "_c
vnoremap c "_c


lua << EOF

require "globals"
require "options"
 --"<cmd>lua require('usermod').C.somefunction()<CR>"
vim.api.nvim_set_keymap("n",
                        "<leader>ht",
                        "<cmd>lua TOGGLE_SHOW_CHAR_LIST()<CR>",
                        { noremap = true })

require "options".setup()
require "plugins/org".setup()
require "plugins/lsp".setup()
require "plugins/autocomplete".setup()
require "plugins/treesitter".setup()
require "plugins/toggleterm".setup()
require "plugins/file_tree".setup()

EOF

