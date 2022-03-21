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

"Treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} 

" Org
Plug 'nvim-orgmode/orgmode'
" LSP
" NO AGREGARRR Plug 'mfussenegger/nvim-jdtls'
Plug 'neovim/nvim-lspconfig'
Plug 'onsails/lspkind-nvim'
Plug 'williamboman/nvim-lsp-installer'

Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'

call plug#end()
" -------------------------------------------------------------------------
"    Plugins END
" -------------------------------------------------------------------------
" Plugins directives
let g:airline_theme='minimalist'

" -------------------------------------------------------------------------
" VIM GENERAL SETTINGS
" -------------------------------------------------------------------------
lua << EOF
require "globals"
require "options"
 --"<cmd>lua require('usermod').C.somefunction()<CR>"
vim.api.nvim_set_keymap("n",
                        "<leader>ht",
                        "<cmd>lua TOGGLE_SHOW_CHAR_LIST()<CR>",
                        { noremap = true })
EOF

"set list of characters to show on invisible characters
"set backspace=indent,eol,start

" filetype on " detect files based on type
" filetype plugin on " when a files is edited, it's plugin file is loaded
" filetype indent on " mantain indentation

" fzf
" set rtp+=/usr/local/bin/fzf
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

" if has('nvim-0.5')
"   augroup lsp
"     au!
"     au FileType java lua require('jdtls').start_or_attach({cmd = {'java-lsp.sh'}})
" 
"   augroup end
" endif

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
nnoremap <leader>fv :20Vexplore<CR>

" Dont copy replaced text in visual mode, you can past the same thing many
" times
xnoremap p "_dP
"
" Changing(or deleting) a word shouldnt alter the default register
nnoremap c "_c
vnoremap c "_c

" -------------------------------------------------------------------------
" LSP
" -------------------------------------------------------------------------


lua << EOF
require'toggleterm'.setup{
  function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  open_mapping = [[<c-\>]],
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  shade_terminals = true,
  shading_factor = '1', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  persist_size = true,
  direction = 'horizontal', -- | 'horizontal' | 'window' | 'float',
  close_on_exit = true, -- close the terminal window when the process exits
  shell = vim.o.shell, -- change the default shell
  -- This field is only relevant if direction is set to 'float'
  float_opts = {
    -- The border key is *almost* the same as 'nvim_open_win'
    -- see :h nvim_open_win for details on borders however
    -- the 'curved' border is a custom border type
    -- not natively supported but implemented in this plugin.
    border = 'single', -- | 'double' | 'shadow' | 'curved' | ... other options supported by win open
    -- width = <value>,
    -- height = <value>,
    winblend = 3,
    highlights = {
      border = "Normal",
      background = "Normal",
    }
  }
}

EOF
" set
let g:toggleterm_terminal_mapping = '<C-t>'
" or manually...
autocmd TermEnter term://*toggleterm#*
      \ tnoremap <silent><c-t> <Cmd>exe v:count1 . "ToggleTerm"<CR>

" By applying the mappings this way you can pass a count to your
" mapping to open a specific window.
" For example: 2<C-t> will open terminal 2
nnoremap <silent><c-t> <Cmd>exe v:count1 . "ToggleTerm"<CR>
inoremap <silent><c-t> <Esc><Cmd>exe v:count1 . "ToggleTerm"<CR>


lua << EOF
 local cmp = require("cmp")
 local lspkind = require('lspkind')

 cmp.setup({
   completion = {
     autocomplete = { },
   },
   snippet = {
     expand = function(args)
       vim.fn["vsnip#anonymous"](args.body)
     end,
   },
   documentation = { },
   sorting = {
     priority_weight = 2.,
     comparators = { },
   },
   mapping = {
     ['<C-y>'] = cmp.mapping.confirm{ select = true },
     -- Defaults from github
     ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
     ['<C-d>'] = cmp.mapping.scroll_docs(-4),
     ['<C-f>'] = cmp.mapping.scroll_docs(4),
     ['<C-Space>'] = cmp.mapping.complete(),
     ['<C-e>'] = cmp.mapping.close(),
     ['<CR>'] = cmp.mapping.confirm({
       behavior = cmp.ConfirmBehavior.Replace,
       select = true,
     })
   },
   sources = {
     { name = "nvim_lsp" },
     { name = "treesitter" },
     { name = "emoji" },
     { name = "orgmode" },
     { name = "buffer" }
   },
  formatting = {
    format = lspkind.cmp_format({
      mode = 'symbol', -- show only symbol annotations
      maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

      -- The function below will be called before any actual modifications from lspkind
      -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
      before = function (entry, vim_item)
        return vim_item
      end
    })
  }})

EOF
autocmd FileType lua lua require'cmp'.setup.buffer {
\   sources = {
\     { name = 'nvim_lua' },
\     { name = 'buffer' }
\   },
\ }

" Org
lua << EOF
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}

require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require "plugins/org".setup()
require "plugins/lsp".setup()
EOF
