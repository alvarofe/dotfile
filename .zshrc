bindkey -e

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.profile

autoload -U compinit && compinit
zmodload -i zsh/complist
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} menu select
xset -b
