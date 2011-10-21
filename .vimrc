" Line numbers
set number
noremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Highlight current line
set cursorline

set smartindent
set tabstop=4
set shiftwidth=4
set encoding=utf8

colorscheme mayansmoke

set expandtab
set expandtab

map ,d :execute 'NERDTreeToggle ' . getcwd()<CR>

set ignorecase
set smartcase
set incsearch

syntax on

let NERDTreeIgnore=['\.o$', '\.hi$']
let NERDTreeSortOrder=['\.hs$', '\.d$', '*', '\.aux$']
