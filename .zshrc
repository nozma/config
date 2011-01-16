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
# setopt auto_cd auto_name_dirs
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob
setopt list_types
setopt no_beep
setopt always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys pushd_ignore_dups
#setopt auto_menu  correct rm_start_silent sum_keyboard_hack
#setopt share_history inc_append_history
# for auto-fu.zsh
unsetopt sh_wordsplit

# Alias and functions {{{1
alias copy='cp -ip' del='rm -i' move='mv -i'
alias fullreset='echo "\ec\ec"'
h () {history $* | less}
alias ja='LANG=ja_JP.eucJP XMODIFERS=@im=kinput2'
alias vi=mvim
alias ls='gls -F --color=auto'
alias la='gls -aF --color=auto'
alias ll='gls -laF --color=auto'
alias emacs='open -a /Applications/Emacs.app'
mdcd () {mkdir -p "$@" && cd "$*[-1]"}
mdpu () {mkdir -p "$@" && pushd "$*[-1]"}
alias pu=pushd po=popd dirs='dirs -v'
# open with Numbers
alias n="open -a /Applications/iWork\ \'09/Numbers.app"

# Suffix aliases {{{1
alias -s pdf=acroread dvi=xdvi
alias -s {odt,ods,odp,doc,xls,ppt}=soffice
alias -s {tgz,lzh,zip,arc}=file-roller

# binding keys {{{1
bindkey -e
#bindkey '^p' history-beginning-search-backward
#bindkey '^n' history-beginning-search-forward

# 補完システムの利用 {{{1
zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
# 大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' completer _oldlist _complete _match _ignored _approximate

autoload -U compinit && compinit

# script {{{1
# auto-fu.zsh {{{2
## auto-fu.zsh stuff.
# source ~/.zsh/script/auto-fu/auto-fu.zsh
{ . ~/.zsh/auto-fu; auto-fu-install; }
zstyle ':auto-fu:highlight' input bold
zstyle ':auto-fu:highlight' completion fg=black,bold
zstyle ':auto-fu:highlight' completion/one fg=white,bold,underline
zstyle ':auto-fu:var' postdisplay $'\n-azfu-'
zle-line-init () {auto-fu-init;}; zle -N zle-line-init

# screenでウィンドウタイトルを自動設定 {{{2
# http://d.hatena.ne.jp/tarao/20100223/1266958660
typeset -ga precmd_functions
typeset -ga preexec_functions
source ~/.zsh/term.zshrc

# rvm {{{2
if [[ -s /Users/rito/.rvm/scripts/rvm ]] ; then source /Users/rito/.rvm/scripts/rvm ; fi
