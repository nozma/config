# .zshenv

setopt no_global_rcs

limit coredumpsize 0
typeset -U path

export PATH=$HOME/local/bin:/opt/local/bin:/usr/local/bin:/usr/X11R6/bin:/usr/bin:/bin:/opt:local/sbin:/usr/local/sbin:/usr/sbin:/sbin:/usr/.local/bin
export MANPATH=/usr/local/share/man/ja_JP.UTF-8:/usr/local/share/man/ja:$MANPATH

export LANG=ja_JP.UTF-8
export LC_CTYPE="$LANG"
export LC_NUMERIC="$LANG"
export LC_TIME=C
export LC_COLLATE="$LANG"
export LC_MONETARY="$LANG"
export LC_MESSAGES="$LANG"
export LC_PAPER="$LANG"
export LC_NAME="$LANG"
export LC_ADDRESS="$LANG"
export LC_TELEPHONE="$LANG"
export LC_MEASUREMENT="$LANG"
export LC_IDENTIFICATION="$LANG"

export EDITOR=vim

export DISPLAY=":0.0"

export RSYNC_RSH=ssh
export CVS_RSH=ssh

# Drag & Drop pTeX for Snow Leopard
export PATH=/Applications/pTeX.app/teTeX/bin:$PATH

# git-osx-installer
# export PATH=/usr/local/git:/usr/local/git/bin:$PATH

#ruby1.9.1
export RUBYLIB=$HOME/.gem/ruby/1.9.1/lib

# perlbrew
#source ~/perl5/perlbrew/etc/bashrc

# mysql
export PATH=/usr/local/mysql/bin:$PATH

# cabal
export PATH=$HOME/.cabal/bin:$PATH

# python
export PYENV_ROOT="${HOME}/.pyenv"
export PATH=${PYENV_ROOT}/bin:$PATH
eval "$(pyenv init -)"

# tex
export PATH=/usr/texbin:$PATH

# Ada
export PATH=/usr/local/gnat/bin:$PATH

# cargo
export PATH=$HOME/.cargo/bin
