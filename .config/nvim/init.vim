" -------------------------------------------------------------------------
"    Plugins START
" -------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'sainnhe/gruvbox-material'

" Plug 'drewtempelmeyer/palenight.vim'
Plug 'fatih/vim-go', { 'do': 'GoUpdateBinaries' }
Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
"
" coc extensions
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'tpope/vim-surround'

Plug 'junegunn/rainbow_parentheses.vim'

" Plug 'joshdick/onedark.vim'
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

let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-yank', 'coc-prettier', 'coc-explorer']

let g:airline_theme='gruvbox_material'

" -------------------------------------------------------------------------
" VIM GENERAL SETTINGS
"
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
" refresh preview on write/normal mode
let g:mkdp_refresh_slow=1

" Configuration below is copied from COC's configuration
"
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Coc-explorer
nmap <leader>fe :CocCommand explorer<CR>

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')


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

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>fs  <Plug>(coc-format-selected)
nmap <leader>fs  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

if has('nvim-0.5')
  augroup lsp
    au!
    au FileType java lua require('jdtls').start_or_attach({cmd = {'java-lsp.sh'}})
  augroup end
endif
" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>ca  :<C-u>CocList actions<cr>
" Show commands.
nnoremap <silent> <space>cc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>cs  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>cj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>ck  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

let g:go_def_mapping_enabled = 0

" For Gvdiffsplit!
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
