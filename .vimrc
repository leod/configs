" Disables arrow keys
"noremap  <Up> ""
"noremap! <Up> <Esc>
"noremap  <Down> ""
"noremap! <Down> <Esc>
"noremap  <Left> ""
"noremap! <Left> <Esc>
"noremap  <Right> ""
"noremap! <Right> <Esc>

" Colemak
noremap n j
noremap e k
noremap i l
xnoremap n j
xnoremap e k
xnoremap i l
onoremap n j
onoremap e k
onoremap i l

noremap l u
noremap L U
noremap u i
noremap U I

noremap k n
noremap K N

noremap <C-t> <C-f>

noremap <C-w>n <C-w>j
noremap <C-w>e <C-w>k
noremap <C-w>i <C-w>l

" Line numbers
set number
noremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Highlight current line
"set cursorline

set smartindent
set tabstop=4
set shiftwidth=4
set encoding=utf8

colorscheme zenburn

au BufEnter *.pas set expandtab

au BufEnter *.hs set expandtab
"au BufEnter *.hs compiler ghc

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
"au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)
""au BufWinEnter * let w:m2=matchadd('ErrorMsg', '^\s\+$', -1)
"nnoremap <silent> <Leader>l
"    \ :if exists('w:m1') <Bar>
"    \   silent! call clearmatches() <Bar>
"    \   unlet w:m1 <Bar>
"    \ else <Bar>
"    \   let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1) <Bar>
"    "\   let w:m2=matchadd('ErrorMsg', '^\s\+$', -1) <Bar>
"    \ endif <CR>

map <F5> :call Compile()<CR>

func! Compile()
	exec "wa"
endfunc
