"起動時ウィンドウ最大化
"au GUIEnter * simalt ~x
"autocmd BufEnter * macaction performZoom:
" 起動時Windowサイズ
set lines=48
set columns=179
" フォント // 使用フォント:Osaka[http://osakattf.hp.infoseek.co.jp/]
if has('win32')
    let &guifont = iconv('Osaka－等幅:h11:cSHIFTJIS', &encoding, 'cp932')
endif
set guifont=Inconsolata:h14

"gui
"カラースキームの設定
colorscheme darkblue
"透過設定
set transparency=18
