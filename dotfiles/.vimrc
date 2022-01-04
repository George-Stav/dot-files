" REQUIRED
set nocompatible
filetype off			" Required for Vundle
syntax enable

" PLUGINS
set rtp+=~/.vim/bundle/Vundle.vim 
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'		" Required to work properly
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'terryma/vim-multiple-cursors'
" Plugin 'neoclide/coc.nvim'

" Web Development
Plugin 'ap/vim-css-color'
Plugin 'mattn/emmet-vim'

" Fancy stuff
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'ryanoasis/vim-devicons'		" Required for nerd fonts to work

call vundle#end()				" Required for Vundle
filetype plugin indent off		" Required for Vundle

" BASIC CONFIGURATION
set number relativenumber
set path+=**
set wildmode=longest,list,full
set encoding=UTF-8
set cursorline
set showmatch			" Showing matching brackets
set linebreak
set ignorecase			" Do case insensitive matching
set smartcase			" Do smart case matching
set clipboard+=unnamedplus	" Use system clipboard. If 'unnamedplus' doesn't work, try 'unnamed'.
set mouse=a			" Enable mouse
set foldenable
set foldmethod=marker
" set foldmarker={{{,}}}
set tabstop=4
set shiftwidth=4
set softtabstop=4
set spelllang=en_gb		" Default language for spell checker
set fillchars+=eob:\

" BASIC STYLING
" colorscheme dracula

highlight Normal ctermbg=NONE
highlight Folded cterm=bold ctermbg=NONE ctermfg=White

highlight Comment cterm=italic
highlight Cursorline ctermbg=Black cterm=NONE
highlight CursorLineNr ctermbg=Black cterm=bold ctermfg=Green
highlight LineNr ctermbg=Black ctermfg=White

highlight SpellBad ctermbg=Red ctermfg=White
highlight SpellCap cterm=NONE ctermbg=NONE
highlight SpellRare cterm=NONE ctermbg=NONE
highlight SpellLocal cterm=Underline ctermbg=NONE

" BASIC KEY BINDING
" Remap Esc Key
inoremap ;; <Esc>

" Map leader key
" let mapleader="Spc"
let mapleader=";"

" Swap current line with lower line
map <leader>x ddp

" Toggle spellchecker
map <leader>s :setlocal spell!<CR>

" Enable and disable auto indent
map <leader>a :setlocal autoindent<CR>
map <leader>A :setlocal noautoindent<CR>

" Enable and disable auto comment
map <leader>c :setlocal formatoptions-=cro<CR>
map <leader>C :setlocal formatoptions=cro<CR>

" VIM AIRLINE CONFIG
let g:airline#extensions#tabline#enabled = 1
