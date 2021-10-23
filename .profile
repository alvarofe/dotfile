export PATH=/home/alvaro/.local/bin:$PATH

alias ta="tmux -2 attach -t"
alias ts="tmux -2 new-session -s"
alias tl="tmux -2 list-sessions"
alias less="less -R"
alias grep="grep --color"

function cs() {
        find . -type f -iname "*.[chS|cc|cpp]" > cscope.files
        ctags -L cscope.files
}

function fixed() {
        echo -e "\033]710;fixed\033\\"
}

alias sd="pushd ."
alias rd="popd"

export EDITOR=nvim
export PAGER=less
export _JAVA_AWT_WM_NONREPARENTING=1
export PKG_CONFIG_PATH=/usr/lib/pkgconfig/:$PKG_CONFIG_PATH
export PATH=$PATH:/home/alvaro/go/bin/
export PATH=/home/alvaro/tools/depot_tools:$PATH
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_so=$'\e[45;93m'
export LESS_TERMCAP_se=$'\e[0m'

alias em="emacsclient -t"
alias ls='ls --color=auto'
export TERM=xterm-256color

PROMPT='%B%F{cyan}%n%f%b@%B%F{yellow}%1d%f%b:~$ '
