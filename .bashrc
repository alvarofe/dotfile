alias ll="ls -alh"
alias l="ls -al"

alias ta="tmux -2 attach -t"
alias ts="tmux -2 new-session -s"
alias tl="tmux -2 list-sessions"
alias tksv="tmux -2 kill kill-server"
alias tkss="tmux -2 kill-session -t"
alias cdr2="cd ~/projects/reverse/radare2/r2"
alias cdr2r="cd ~/projects/reverse/radare2/r2-regressions"
alias tags="ctags -R --c++-kinds=+p --fields=+iaS --extra=+q ." 
alias less="less -R"
alias grep="grep --color"

function cs() {
	find . -name "*.c" -o -name "*.cc" -o -name "*.h" -o -name "*.hpp" -o -name "*.cpp" > cscope.files
	cscope -b
}
export CLICOLOR=1
#export LSCOLORS=GxFxCxDxBxegedabagaced
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export EDITOR=vim
export PAGER=less
export PS1="\h:\W \u\$ "
