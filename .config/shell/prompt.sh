# Set different color for host on SSH
__setup_prompt() {
    local b_yellow b_green b_red n_blue f_reset
    if [[ -n ${ZSH_VERSION-} ]]; then
        b_red="%B%F{9}"
        b_green="%B%F{10}"
        b_yellow="%B%F{11}"
        b_blue="%B%F{12}"
        n_blue="%F{12}"
        f_reset="%f%b"
    else
        b_red="\[\e[1;91m\]"
        b_green="\[\e[1;92m\]"
        b_yellow="\[\e[1;93m\]"
        b_blue="\[\e[1;94m\]"
        n_blue="\[\e[0;94m\]"
        f_reset="\[\e[0m\]"
    fi
    if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
        __prompt_host="$b_yellow$1$f_reset"
    else
        __prompt_host="$b_green$1$f_reset"
    fi
    __prompt_host="$__prompt_host${NIX_PATH:+ on ${b_blue}nix$f_reset}"
    __prompt_dir="$b_red$2$f_reset"
    __prompt_prompt="$n_blue$3$f_reset"
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
. ~/.config/shell/git-prompt/git-prompt.sh
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
