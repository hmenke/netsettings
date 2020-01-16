# Print the time the last command took
__timer_reset() {
    __timer_diff=0
    __timer_ready=true
}

__timer_start() {
    if [ "$__timer_ready" = true ]; then
        __timer_ready=false
        __timer=${__timer:-$SECONDS}
    fi
}

__timer_stop() {
    if [ -n "$__timer" ]; then
        __timer_diff=$((SECONDS - __timer))
        unset __timer
    fi
}

__timer_show() {
    if [ $__timer_diff -gt 1 ]; then
        echo -ne " took ${__timer_diff}s"
    fi
}

# Print the exit status if it was non-zero
__last_status=0

__show_status() {
    if [ $__last_status -ne 0 ]; then
        echo -ne " exit $__last_status"
    fi
}

# Show the git branch in the prompt
source ~/.config/shell/git-prompt/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWUPSTREAM=auto
