" Basic "{{{1
" Options "{{{2

filetype plugin indent on
syntax enable
set nocompatible
set shell=zsh
" set spell
" set spelllang=en_us  

" Directory "{{{2

set backup
set backupdir=~/vimbackup
set directory=~/vimbackup

" Encodings "{{{2
" To deal with Japanese language. 
if $ENV_WORKING ==# 'summer'
  set encoding=japan
else
  set encoding=utf-8
endif
 
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
 
  " Does iconv support JIS X 0213 ?
  if iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213,euc-jp'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
 
  " Make fileencodings
  let &fileencodings = 'ucs-bom'
  if &encoding !=# 'utf-8'
    let &fileencodings = &fileencodings . ',' . 'ucs-2le'
    let &fileencodings = &fileencodings . ',' . 'ucs-2'
  endif
  let &fileencodings = &fileencodings . ',' . s:enc_jis
 
  if &encoding ==# 'utf-8'
    let &fileencodings = &fileencodings . ',' . s:enc_euc
    let &fileencodings = &fileencodings . ',' . 'cp932'
  elseif &encoding =~# '^euc-\%(jp\|jisx0213\)$'
    let &encoding = s:enc_euc
    let &fileencodings = &fileencodings . ',' . 'utf-8'
    let &fileencodings = &fileencodings . ',' . 'cp932'
  else " cp932
    let &fileencodings = &fileencodings . ',' . 'utf-8'
    let &fileencodings = &fileencodings . ',' . s:enc_euc
  endif
  let &fileencodings = &fileencodings . ',' . &encoding
 
  unlet s:enc_euc
  unlet s:enc_jis
endif
 
 
if $ENV_ACCESS ==# 'summer'
  set termencoding=cp932
else " fallback
  set termencoding= " same as 'encoding'
endif

" Key Mappings "{{{2

noremap j gj
noremap k gk
noremap gj j
noremap gk k
noremap <C-h> :<C-u>help<Space>
" time stamp
inoremap <expr> ,df strftime('%Y-%m-%d %H:%M:%S')
inoremap <expr> ,dd strftime('%Y-%m-%d')
inoremap <expr> ,dt strftime('%H:%M:%S')
" " open _vimrc
" nnoremap <Space>. :<C-u>edit $MYVIMRC<Enter>
" " open _gvimrc
" nnoremap <Space>g. :<C-u>edit $MYGVIMRC<Enter>
" " save _vimrc and reload
" nnoremap <Space>s. :<C-u>w<Enter>:<C-u>source $MYVIMRC<Enter>
" " save _gvimrc and reload
" nnoremap <Space>gs. :<C-u>w<Enter>:<C-u>source $MYGVIMRC<Enter>
" insert blank line [http://vim-users.jp/2009/08/hack57/]
nnoremap O :<C-u>call append(expand('.'), '')<Cr>j
" scroll-smooth
map <C-U> <C-Y>2<C-Y>2<C-Y>2<C-Y>2<C-Y><C-Y>
map <C-D> <C-E>2<C-E>2<C-E>2<C-E>2<C-E><C-E>
" open new tab
imap ,t :tabnew

" Search and Input "{{{2

autocmd FileType help nnoremap <buffer> q <C-w>c
set backspace=eol,indent,start
set ignorecase
set smartcase
set incsearch
set hlsearch
set showmode
set showcmd
set whichwrap=b,s,h,l,<,>,[,]
set clipboard=unnamed
" Highlight status line
set list
set listchars=tab:^_
augroup InsertHook
autocmd!
autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=#2E4340
autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ccdc90
augroup END
" Tab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
" automatic formatting
"set textwidth=72
set textwidth=0
set formatoptions=qmM

" View "{{{2

set showtabline=2
set list
set number
set foldmethod=marker
set laststatus=2
set statusline=%F%m%r%h%w\%=[TYPE=%Y]\[FORMAT=%{&ff}]\[ENC=%{&fileencoding}]\[LOW=%l/%L,%3p%%]

" Edit {{{1

" CSV {{{2

" Highlight a column in csv text.
" :Csv 12   " highlight twelfth column
" :Csv 0    " switch off highlight
function! CSVH(colnr)
  if a:colnr > 1
    let n = a:colnr - 1
    execute 'match Keyword /^\([^,]*,\)\{'.n.'}\zs[^,]*/'
    execute 'normal! 0'.n.'f,'
  elseif a:colnr == 1
    match Keyword /^[^,]*/
    normal! 0
  else
    match
  endif
endfunction
command! -nargs=1 Csv :call CSVH(<args>)

" Highlighting the header row 
syntax match csvHeading /\%1l\%(\%("\zs\%([^"]\|""\)*\ze"\)\|\%(\zs[^,"]*\ze\)\)/
highlight def link csvHeading Type

" perl {{{2

autocmd FileType perl,cgi :compiler perl
nnoremap <C-p> :w !perl<CR>

" commentstring {{{2

autocmd Filetype hatena set commentstring=<!--%s-->
autocmd Filetype R set commentstring=#%s
autocmd Filetype lisp set commentstring=;;%s

" Plugin "{{{1
" neocomplcache http://github.com/Shougo/neocomplcache {{{2
let g:neocomplcache_enable_at_startup = 1
" quickrun.vim {{{2

" Align.vim{{{2

" Fin. "{{{1
set secure

" __END__ "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
