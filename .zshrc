# Main {{{1
HISTFILE=$HOME/.zsh-history
HISTSIZE=50000
SAVEHIST=50000
export OUTPUT_CHARSET=utf-8
export LANG=ja_JP.UTF-8
export PAGER="lv -cOu8"
export LESS=mqeisz-2XR

# Prompt {{{1
setopt prompt_subst
PROMPT='%{'$'\e[''$[36]m%}%U%B%m{%n}%b%{'$'\e[''m%}%U%%%u '
RPROMPT='%{'$'\e[''33m%}[%~]%{'$'\e[''m%}'

# Set shell options {{{1
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob
setopt list_types
setopt no_beep
setopt always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys pushd_ignore_dups

# Alias and functions {{{1
alias copy='cp -ip' del='rm -i' move='mv -i'
alias fullreset='echo "\ec\ec"'
h () {history $* | less}
alias ja='LANG=ja_JP.eucJP XMODIFERS=@im=kinput2'
alias vim=mvim
alias vi='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias ls='gls -F --color=auto'
alias la='gls -aF --color=auto'
alias ll='gls -laF --color=auto'

# Suffix aliases {{{1
alias -s pdf=acroread dvi=xdvi
alias -s {odt,ods,odp,doc,xls,ppt}=soffice
alias -s {tgz,lzh,zip,arc}=file-roller

# 補完システムの利用 {{{1
zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
# 大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' completer _oldlist _complete _match _ignored _approximate

autoload -U compinit && compinit

fpath=(/usr/local/share/zsh-completions $fpath)

# script {{{1
# zsh-autosuggestions(https://github.com/zsh-users/zsh-autosuggestions) {{{2
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# screenでウィンドウタイトルを自動設定 {{{2
# http://d.hatena.ne.jp/tarao/20100223/1266958660
typeset -ga precmd_functions
typeset -ga preexec_functions
