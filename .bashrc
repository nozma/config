export SHELL=/bin/zsh
export HOME=~
export TMPDIR=/tmp
export TZ=JST-09
export MAKE_MODE=unix

# LANG Options
export LANG=ja_JP.UTF-8
export JLESSCHARSET=japanese-sjis

# prompt
PS1="\[\e[36m\]\u@\h:\w \\$ \[\e[0m\]"
# PS1='\033k\033\\[\u@\h \W]\$ '

# alias
alias vi=mvim
alias ls="gls -F --color=auto"
alias la="gls -aF"
alias ll="gls -la"

# PATH
PATH=$HOME/local/bin:/opt/local/bin:/usr/local/bin:/usr/X11R6/bin:/usr/bin:/bin:/opt:local/sbin:/usr/local/sbin:/usr/sbin:/sbin
PATH=/Applications/pTex.app/teTeX/bin:$PATH
export PATH
PAGER=less
EDITOR=vim
export PAGER EDITOR

export DISPLAY=":0.0"
