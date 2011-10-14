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

" Highlight lines that are longer than 80 chars
" Highlight lines that are only whitespace
" toggle with \l
au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)
au BufWinEnter * let w:m2=matchadd('ErrorMsg', '^\s\+$', -1)
nnoremap <silent> <Leader>l
    \ :if exists('w:m1') <Bar>
    \   silent! call clearmatches() <Bar>
    \   unlet w:m1 <Bar>
    \ else <Bar>
    \   let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1) <Bar>
    \   let w:m2=matchadd('ErrorMsg', '^\s\+$', -1) <Bar>
    \ endif <CR>
