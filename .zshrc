bindkey -e

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.profile

HISTFILE="${HISTFILE:-${ZDOTDIR:-$HOME}/.zsh_history}"
HISTSIZE=1000
SAVEHIST=1000

PROMPT='%B%F{cyan}%n%f%b@%B%F{yellow}%1d%f%b:~$ '
autoload -U compinit && compinit
zmodload -i zsh/complist
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} menu select
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

if [[ -v INSIDE_EMACS ]]; then
    export TERM="eterm-color"
fi
