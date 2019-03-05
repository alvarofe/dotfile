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
	find -L . -name "*.c" -o -name "*.cc" -o -name "*.h" -o -name "*.hpp" -o -name "*.cpp" > cscope.files
	cscope -b -q
	ctags -L cscope.files
}

function kernel_tags() {
    rm -rf cscope.*
    rm -rf tags

    find . -path ./arch -prune -o -path ./tools -prune -o -type f -name "*.[chsS]" -print >> cscope.files
    find arch/$1 -type f -name "*.[chsS]" -print >> cscope.files

    cscope -b -q -k
	ctags -L cscope.files
}

export CLICOLOR=1
#export LSCOLORS=GxFxCxDxBxegedabagaced
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export EDITOR=vim
export PAGER=less
export PS1="\h:\W \u\$ "


export CFLAGFUZZ="-fsanitize=address -fsanitize-coverage=bb,indirect-calls,trace-pc-guard,trace-cmp,trace-div,trace-gep -fno-omit-frame-pointer"
export PATH=$PATH:/home/alvaro/dotfile/i3/scripts

export AFL_PATH="/home/alvaro/tools/afl-latest"
export LSAN_OPTIONS=verbosity=0:log_threads=0
export ASAN_OPTIONS=detect_leaks=0,abort_on_error=1,symbolize=0,detect_odr_violation=0
export PATH=$PATH:/home/alvaro/projects/android/toolchain/x86_64-linux-android-4.9/bin

function get_service_number {                                                                                                                                                                                                                                                                                                 
    cat $1 | gcc -P -E - | tr '{};\n\r' '\n\n\n  ' | grep -v ^$ | sed -e '1,/interface\s/ d' | sed '/^\s*$/d' | cat -n
}


alias aosp="cd /home/alvaro/projects/android/aosp" 
export PATH=/home/alvaro/tools/llvm/llvm-project/build/bin:$PATH
export PATH=$PATH:/home/alvaro/Android/android-ndk-r19
export PATH=$PATH:/home/alvaro/.local/bin
alias em=emacs

