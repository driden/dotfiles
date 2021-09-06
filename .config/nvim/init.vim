" -------------------------------------------------------------------------
"    Plugins START
" -------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

" THEMES

" Plug 'drewtempelmeyer/palenight.vim'
" Plug 'joshdick/onedark.vim'
" Plug 'sainnhe/gruvbox-material'
Plug 'NLKNguyen/papercolor-theme'
Plug 'rktjmp/lush.nvim'
Plug 'metalelf0/jellybeans-nvim'


" Plug 'fatih/vim-go', { 'do': 'GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

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
let g:airline_theme='minimalist'

" -------------------------------------------------------------------------
" VIM GENERAL SETTINGS
" -------------------------------------------------------------------------
lua << EOF
require "globals"
require "options"
EOF

"set list of characters to show on invisible characters
" set backspace=indent,eol,start

filetype on " detect files based on type
filetype plugin on " when a files is edited, it's plugin file is loaded
filetype indent on " mantain indentation

" fzf
set rtp+=/usr/local/bin/fzf
" leader
let mapleader=" "
" Always show line status
" set laststatus=2

"let g:gruvbox_material_background = 'hard' "soft,medium, hard
colorscheme PaperColor 

augroup rainbow_parens
  autocmd!
  autocmd FileType javascript,typescript,json,go RainbowParentheses
augroup END

command JqBuffer execute "%!jq"

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
nnoremap <leader>tc <C-\><C-n>:q<CR>

" Edit this file
"
nnoremap <leader>fp :e ~/.config/nvim/init.vim<CR>

" Explorer 
nnoremap <leader>fv :20Vexplore<CR>

" Dont copy replaced text in visual mode, you can past the same thing many
" times
xnoremap p "_dP

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

