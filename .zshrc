bindkey -e

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.profile

HISTFILE="${HISTFILE:-${ZDOTDIR:-$HOME}/.zsh_history}"
HISTSIZE=1000
SAVEHIST=1000

autoload -U compinit && compinit
zmodload -i zsh/complist

function git_branch_name()
{
  branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
  if [[ $branch == "" ]];
  then
    :
  else
    echo $branch
  fi
}

rga-fzf() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	xdg-open "$file"
}


setopt PROMPT_SUBST
PROMPT='%B%F{cyan}%1n@%B%F{yellow}%1m%f%b:%B%F{blue}%2~%f%b:%B%F{166}$(git_branch_name)%f%b -%B%F{cyan}$%f%b '

zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} menu select
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

if [[ -v INSIDE_EMACS ]]; then
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
    export TERM="eterm-color"
else
    export TERM="xterm-256color"
fi

