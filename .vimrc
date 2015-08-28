""""""""""""""""""""""""
execute pathogen#infect()

""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""
" Airline
let g:airline_powerline_fonts = 1                     " Better look of airline
let g:airline#extensions#tabline#enabled = 1          " Tabline for buffers

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_haskell_checkers = ['hlint', 'ghc_mod']
""""""""""""""""""""""""
" General
""""""""""""""""""""""""
set encoding=utf-8

" Allow Syntax Highlighting and Filetype Plugins
syntax on
filetype on
filetype plugin indent on

""""""""""""""""""""""""
" Behaviour
""""""""""""""""""""""""
" Auto Reload a File when changed outside
set autoread

" Indentation
set smartindent
set tabstop=2                 " Number of spaces per tab
set shiftwidth=2
set expandtab                 " Use spaces instead of tabs
set smarttab
set backspace=eol,start,indent " Clever use of backspace

" UI
set number                    " Show Line Numbers
set ruler                     " Show Current Position
set showcmd

set wildmenu                  " Autocompletion
set wildignore=*.o,*~,*.ibc   " Ignore Compiled files

" Search
set ignorecase                " No case sensitivity when searching
set smartcase                 " Smart Search
set hlsearch                  " Highlight Search Results
set incsearch                 " Modern Search

" Mouse Control
set mouse=a

" Cursor Movement
map j gj
map k gk

set magic

set showmatch                 " Show matching brackets

set laststatus=2

""""""""""""""""""""""""
" Colors
""""""""""""""""""""""""
colorscheme desert
set background=dark
set t_Co=256                  " 256 Colors

""""""""""""""""""""""""
" Files  and Backups
""""""""""""""""""""""""
set nobackup
set noswapfile

""""""""""""""""""""""""
" Commands and shortcuts
""""""""""""""""""""""""
cmap w!! w !sudo tee > /dev/null %

" Tabs
cnoreabbrev t tabedit
