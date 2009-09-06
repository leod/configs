" Disables arrow keys
"noremap  <Up> ""
"noremap! <Up> <Esc>
"noremap  <Down> ""
"noremap! <Down> <Esc>
"noremap  <Left> ""
"noremap! <Left> <Esc>
"noremap  <Right> ""
"noremap! <Right> <Esc>

" Line numbers
set number
noremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Highlight current line
"set cursorline

set smartindent
set tabstop=4
set shiftwidth=4
set encoding=utf8

colorscheme xoria256

au BufEnter *.pas set expandtab

au BufEnter *.hs set expandtab
"au BufEnter *.hs compiler ghc

map ,d :execute 'NERDTreeToggle ' . getcwd()<CR>

set ignorecase
set smartcase
set incsearch

let NERDTreeIgnore=['\.o$', '\.hi$']
let NERDTreeSortOrder=['\.hs$', '\.d$', '*', '\.aux$']
