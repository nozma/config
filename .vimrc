" Basic "{{{1
" Initialize "{{{2

" Vim終了時にウィンドウタイトルを「Vimを使ってくれてありがとう」に変更しない
" http://qiita.com/ttdoda/items/903e85f07d58018c851d
let &t_ti .= "\e[22;0t"
let &t_te .= "\e[23;0t"

" Options "{{{2

filetype off
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
set   encoding=utf-8
set   fenc=utf-8

if has('win32') && has('kaoriya')
  set   ambiwidth=auto
else
  set   ambiwidth=double
endif

if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'

  if iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213,euc-jp'
    let s:enc_jis = 'iso-2022-jp-3'
  endif

  set   fileencodings&
  let &fileencodings = &fileencodings.','.s:enc_jis.',cp932,'.s:enc_euc

  unlet s:enc_euc
  unlet s:enc_jis
endif

if has('win32unix')
  set   termencoding=cp932
elseif !has('macunix')
  set   termencoding=euc-jp
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

" python {{{2

autocmd FileType python setl autoindent
autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType python setl tabstop=8 expandtab shiftwidth=4 softtabstop=4

" commentstring {{{2

autocmd Filetype hatena set commentstring=<!--%s-->
autocmd Filetype R set commentstring=#%s
autocmd Filetype lisp set commentstring=;;%s

" rust {{{2
let g:rustfmt_autosave = 1
let g:rustfmt_command = '$HOME/.cargo/bin/rustfmt'

" Plugin "{{{1
" vundle(off) {{{2

"set nocompatible               " be improved
"filetype off                   " required!
"set rtp+=~/.vim/bundle/vundle/
"call vundle#rc()
"" let vundle manage vundle
"" required!
"bundle 'gmarik/vundle'
"" my bundles here:
"bundle 'shougo/unite.vim'
"bundle 'shougo/vimfiler'
"bundle 'quickrun.vim'
"" original repos on github
"bundle 'tpope/vim-fugitive'
"bundle 'lokaltog/vim-easymotion'
"bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"bundle 'tpope/vim-rails.git'
"" vim-scripts repos
"bundle 'l9'
"bundle 'fuzzyfinder'
"" non github repos
"bundle 'git://git.wincent.com/command-t.git'
"" ...
"filetype plugin indent on     " required!
""
"" Brief help
"" :BundleList          - list configured bundles
"" :BundleInstall(!)    - install(update) bundles
"" :BundleSearch(!) foo - search(or refresh cache first) for foo
"" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
""
"" see :h vundle for more details or wiki for FAQ
"" NOTE: comments after Bundle command are not allowed..

" neobundle {{{2
" neobundle settings {{{
if has('vim_starting')
  set nocompatible
  " neobundle をインストールしていない場合は自動インストール
  if !isdirectory(expand("~/.vim/bundle/neobundle.vim/"))
    echo "install neobundle..."
    " vim からコマンド呼び出しているだけ neobundle.vim のクローン
    :call system("git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim")
  endif
  " runtimepath の追加は必須
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#begin(expand('~/.vim/bundle'))
let g:neobundle_default_git_protocol='https'

" neobundle#begin - neobundle#end の間に導入するプラグインを記載します。
NeoBundleFetch 'Shougo/neobundle.vim'
" ↓こんな感じが基本の書き方
NeoBundle 'nanotech/jellybeans.vim'

" my bundles here:
NeoBundle 'Shougo/unite.vim'
NeoBundle 'quickrun.vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'moznion/hateblo.vim'
NeoBundle 'kana/vim-metarw'

" vimrc に記述されたプラグインでインストールされていないものがないかチェックする
NeoBundleCheck
call neobundle#end()

filetype plugin on
filetype indent on

" unite.vim {{{2
" 入力モードで開始する
" let g:unite_enable_start_insert=1
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>Unite -buffer-name=files file<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
" 最近使用したファイル一覧
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
" 常用セット
nnoremap <silent> ,uu :<C-u>Unite buffer file_mru<CR>
" 全部乗せ
nnoremap <silent> ,ua :<C-u>UniteWithCurrentDir -buffer-name=files buffer file_mru bookmark file<CR>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

" neocomplcache http://github.com/Shougo/neocomplcache {{{2
let g:neocomplcache_enable_at_startup = 1
" quickrun.vim {{{2

" Align.vim{{{2

" Fin. "{{{1
set secure

" __END__ "{{{1
" vim: expandtab softtabstop=2 shiftwidth=2
" vim: foldmethod=marker
