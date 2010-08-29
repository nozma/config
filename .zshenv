# .zshenv

limit coredumpsize 0
typeset -U path

export PATH=$HOME/local/bin:/opt/local/bin:/usr/local/bin:/usr/X11R6/bin:/usr/bin:/bin:/opt:local/sbin:/usr/local/sbin:/usr/sbin:/sbin:/usr/.local/bin
export MANPATH=$HOME/local/man:opt/local/man:usr/local/man:/usr/share/man:/usr/X11R6/man

export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
export EDITOR=vim

export DISPLAY=":0.0"

export RSYNC_RSH=ssh
export CVS_RSH=ssh

# Drag & Drop pTeX for Snow Leopard
export PATH=/Applications/pTeX.app/teTeX/bin:$PATH

# git-osx-installer
export PATH=/usr/local/git:/usr/local/git/bin:$PATH

#ruby1.9.1
export RUBYLIB=$HOME/.gem/ruby/1.9.1/lib

# perlbrew
source /Users/rito/perl5/perlbrew/etc/bashrc
