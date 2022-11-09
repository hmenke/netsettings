# Clear ZSH's right prompt
RPROMPT=""

# Set different color for host on SSH
__setup_prompt() {
	local b_yellow b_green b_red n_blue f_reset
	if [[ -n ${ZSH_VERSION-} ]]; then
		b_red="%B%F{1}"
		b_green="%B%F{2}"
		b_yellow="%B%F{3}"
		b_blue="%B%F{4}"
		n_blue="%F{4}"
		f_reset="%f%b"
	else
		b_red="\[\e[1;31m\]"
		b_green="\[\e[1;32m\]"
		b_yellow="\[\e[1;33m\]"
		b_blue="\[\e[1;34m\]"
		n_blue="\[\e[0;34m\]"
		f_reset="\[\e[0m\]"
	fi
	if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
		__prompt_host="$b_yellow$1$f_reset"
	else
		__prompt_host="$b_green$1$f_reset"
	fi
	__prompt_host="$__prompt_host${NIX_PATH:+ on ${b_blue}nix$f_reset}${IN_NIX_SHELL:+ in ${b_yellow}$IN_NIX_SHELL shell$f_reset}"
	__prompt_dir="$b_red$2$f_reset"
	__prompt_prompt="$n_blue$3$f_reset"
}

# Print the time the last command took
__timer_reset() {
	__timer_diff=0
	__timer_ready=true
	unset __timer
}

__timer_start() {
	local prev_last_arg="$1"
	if [ -n ${__timer_ready+x} ]; then
		unset __timer_ready
		__timer=${__timer:-$SECONDS}
	fi
	: "$prev_last_arg"
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

# Fix VTE prompt command to preserve exit status
__vte_prompt_command_() {
	local previous_exit_status=$?
	__vte_prompt_command
	return $previous_exit_status
}

# git-prompt

# fallback if git-prompt.sh cannot be sourced
__git_ps1 () {
	local exit=$?
	local ps1pc_start='\u@\h:\w '
	local ps1pc_end='\$ '
	local printf_format=' (%s)'

	case "$#" in
		2|3)	ps1pc_start="$1"
			ps1pc_end="$2"
		;;
	esac
	PS1="$ps1pc_start$ps1pc_end"
	return $exit
}

# try to find git-prompt.sh
if [ -f ~/.config/shell/git-prompt.sh ]; then
	. ~/.config/shell/git-prompt.sh
elif [ -f /etc/bash_completion.d/git-prompt ]; then
	. /etc/bash_completion.d/git-prompt
elif [ -f /usr/lib/git-core/git-sh-prompt ]; then
	. /usr/lib/git-core/git-sh-prompt
fi

# git-prompt settings
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWUPSTREAM=auto
export GIT_PS1_TIMEOUT_VERBOSE=1
export GIT_PS1_SHOWCONFLICTSTATE=yes

# The main prompt drawing function
__draw_prompt() {
	__last_status=$?
	__timer_stop
	__git_ps1 "╭╴${__prompt_host} ${__prompt_dir}" "$(__timer_show)$(__show_status)
╰╴${__prompt_prompt} "
	__prompt_posthook
	__timer_reset
	return $last_status
}

__prompt_posthook() { return; }
