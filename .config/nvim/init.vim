" -------------------------------------------------------------------------
"    Plugins START
" -------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

" Plug 'drewtempelmeyer/palenight.vim'
" Plug 'joshdick/onedark.vim'
Plug 'sainnhe/gruvbox-material'

Plug 'fatih/vim-go', { 'do': 'GoUpdateBinaries' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-surround'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

"Treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} 

" LSP
Plug 'mfussenegger/nvim-jdtls'
Plug 'neovim/nvim-lspconfig'

call plug#end()
" -------------------------------------------------------------------------
"    Plugins END
" -------------------------------------------------------------------------
" Plugins directives
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'],['{', '}']]
let g:rainbow#blacklist = [59, 238, 248]
let g:airline_theme='gruvbox_material'

" -------------------------------------------------------------------------
" VIM GENERAL SETTINGS
" -------------------------------------------------------------------------

"set list of characters to show on invisible characters
set listchars=tab:>·,trail:~,extends:>,precedes:<,space:.
set nolist

set hidden " hidden buffers

" no backup
set nobackup
set nowritebackup

set cmdheight=2 " Give more space for displaying messages.

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300
set shortmess+=c " Don't pass messages to |ins-completion-menu|.
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes
syntax on " turn on syntax highlighting
set clipboard=unnamedplus " share clipboard with OS
set cursorline " set cursorline
set nowrap " no wrapping
set relativenumber " set line number
set nocompatible " not compatible with earlier versions
set ignorecase " always start with ignorecase option on
set autoindent " make vim use the indent of the previous line for a newly created one.
set tabstop=4 " show existing tab with 4 spaces width
set shiftwidth=2 " when indenting with '>' use 2 spaces width
set expandtab " change tab to spaces
set incsearch " Display matches for a search pattern while you type.
set background=dark " dark background
" delete the white space at the start of the line, a line break and the
" character before where Insert mode started.
set backspace=indent,eol,start

filetype on " detect files based on type
filetype plugin on " when a files is edited, it's plugin file is loaded
filetype indent on " mantain indentation

" show mode
set noshowmode

" fzf
set rtp+=/usr/local/bin/fzf
" leader
let mapleader=" "
" Always show line status
" set laststatus=2

set termguicolors
let g:gruvbox_material_background = 'hard' "soft,medium, hard
colorscheme gruvbox-material

augroup rainbow_parens
  autocmd!
  autocmd FileType javascript,typescript,json,go RainbowParentheses
augroup END

lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ignore_install = { }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = {},  -- list of language that will be disabled
  },
}
EOF

if has('nvim-0.5')
  augroup lsp
    au!
    au FileType java lua require('jdtls').start_or_attach({cmd = {'java-lsp.sh'}})

  augroup end
endif

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
nnoremap <Leader>pv :Explore<CR>

" Windows
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>
nnoremap <leader>wq :wincmd q<CR>

" Buffers
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

" -------------------------------------------------------------------------
" LSP
" -------------------------------------------------------------------------

" C#
lua << EOF
local lspconfig = require'lspconfig'
local pid = vim.fn.getpid();
local omnisharp_bin = "/usr/local/Cellar/omnisharp/1.35.3/libexec/run"

lspconfig.omnisharp.setup {
    cmd = { omnisharp_bin, "--languageserver" , "--hostPID", tostring(pid) };
    filetypes = { "cs", "vb" };
    init_options = {};
    root_dir = lspconfig.util.root_pattern("../.csproj", "../.sln");
}

EOF

" Terraform
" https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#terraformls

lua << EOF
local lsp = require'lspconfig'
require'lspconfig'.terraformls.setup{
  cmd = { "terraform-ls", "serve" },
  filetypes = { "tf", "terraform", "hcl" },
  root_dir = lsp.util.root_pattern(".git")
}

EOF

