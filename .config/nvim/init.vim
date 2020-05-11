" -------------------------------------------------------------------------
"    Plugins START
" -------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }

call plug#end()
" -------------------------------------------------------------------------
"    Plugins END
" -------------------------------------------------------------------------

" turn on syntax highlighting
syntax on

" set cursorline
set cursorline

" no wrapping
set nowrap

" set line number
set number

" not compatible with earlier versions
set nocompatible

" always start with ignorecase option on
set ignorecase

" delete the white space at the start of the line, a line break and the
" character before where Insert mode started.
set backspace=indent,eol,start

" make vim use the indent of the previous line for a newly created one.
set autoindent

" show mode
set showmode

" Display matches for a search pattern while you type.
set incsearch

" dark background
set background=dark

" theme
set termguicolors
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox

" refresh preview on write/normal mode
let g:mkdp_refresh_slow=1
