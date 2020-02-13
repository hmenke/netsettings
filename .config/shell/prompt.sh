# Set different color for host on SSH
__setup_prompt() {
    if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
        __prompt_host="[1;93m$1[0m"
    else
        __prompt_host="[1;92m$1[0m"
    fi
    __prompt_dir="[1;91m$2[0m"
    __prompt_prompt="[0;94m$3[0m"
}

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

# The main prompt drawing function
__draw_prompt() {
    __last_status="$?"
    __timer_stop
    __git_ps1 "╭╴${__prompt_host} ${__prompt_dir}" "$(__timer_show)$(__show_status)
╰╴${__prompt_prompt} "
    __timer_reset
    __prompt_posthook
}

__prompt_posthook() { return; }
