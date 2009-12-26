# Main {{{1
autoload -U compinit
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

# Alias {{{1
alias vi=mvim
alias ls='gls -F --color=auto'
alias la='gls -aF --color=auto'
alias ll='gls -laF --color=auto'
