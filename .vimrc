" Basic "{{{1
" Options "{{{2

filetype plugin indent on
syntax enable
set nocompatible
set shell=zsh

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
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
" automatic formatting
set textwidth=72
set formatoptions=qmM

" View "{{{2

set list
set number
set foldmethod=marker
set laststatus=2
set statusline=%F%m%r%h%w\%=[TYPE=%Y]\[FORMAT=%{&ff}]\[ENC=%{&fileencoding}]\[LOW=%l/%L,%3p%%]

" Language {{{1
" perl{{{2

autocmd FileType perl,cgi :compiler perl
nnoremap <C-p> :w !perl<CR>

" commentstring {{{2
autocmd Filetype hatena set commentstring=<!--%s-->
autocmd Filetype R set commentstring=#%s
autocmd Filetype lisp set commentstring=;;%s

" Plugin "{{{1
" skk.vim "{{{2

" http://www.vim.org/scripts/script.php?script_id=1589
let skk_jisyo ='~/.skk-jisyo'
let skk_large_jisyo = '~/vimfiles/dict/skk/SKK-JISYO.L'
let skk_auto_save_jisyo = 1
let skk_keep_state = 0
let skk_egg_like_newline = 1
let skk_show_annotation = 1
let skk_use_face = 1

" VIM-LaTeX {{{2
set shellslash
set grepprg=grep\ -nH\ $*
let g:Tex_CompileRule_dvi = 'platex -interaction=nonstopmode $*'
let g:Tex_BibtexFlavor = 'jbibtex'
let g:Tex_ViewRule_dvi = 'c:/tex/dviout/dviout.exe'
let g:Tex_FormatDependency_pdf = 'dvi,pdf'
let g:Tex_CompileRule_pdf = 'dvipdfmx $*.dvi'
"let g:Tex_ViewRule_pdf = 'C:\Program Files\Adobe\Reader 9.0\Reader\AcroRd32.exe'
let g:Tex_ViewRule_pdf = 'C:\Program Files\SumatraPDF\SumatraPDF.exe'
let g:Tex_DefaultTargetFormat = 'pdf'

" quickrun.vim {{{2

" Fin. "{{{1
set secure

" __END__ "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
