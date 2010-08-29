# Main {{{1
autoload -U compinit && compinit
compinit -u
HISTFILE=$HOME/.zsh-history
HISTSIZE=50000
SAVEHIST=50000
export OUTPUT_CHARSET=utf-8
export LANG=ja_JP.UTF-8
export PAGER="lv -Ou8"

# Prompt {{{1
setopt prompt_subst
PROMPT='%{'$'\e[''$[36]m%}%U%B%m{%n}%b%{'$'\e[''m%}%U%%%u '
RPROMPT='%{'$'\e[''33m%}[%~]%{'$'\e[''m%}'

# Set shell options {{{1
setopt auto_cd auto_remove_slash auto_name_dirs
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob list_types no_beep always_last_prompt
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
alias emacs='open -a Emacs.app'
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

# script {{{1
# auto-fu.zsh
source ~/.zsh/script/auto-fu.zsh; zle-line-init () { auto-fu-init; }; zle -N zle-line-init

# rvm
if [[ -s /Users/rito/.rvm/scripts/rvm ]] ; then source /Users/rito/.rvm/scripts/rvm ; fi
