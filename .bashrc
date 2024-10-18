# Exit if already sourced
if [ -n "${__BASHRC_PROFILE_SOURCED+x}" ]; then return; fi
__BASHRC_PROFILE_SOURCED=1

# Source environment variables
. ~/.config/shell/environment.sh

# Exit if not interactive
case $- in
	*i*) ;;
	*) return ;;
esac

# Load the Git completion eagerly (this has to be done before setting up aliases)
if declare -F _completion_loader >/dev/null && ! declare -F __git_complete __git_main >/dev/null; then
	_completion_loader git
fi

# source
__try_source() {
	if [[ -f "$1" ]]; then
		. "$1"
		return $?
	fi
	return 1
}

# Source common configuration
__try_source /etc/profile
__try_source /etc/bashrc
__try_source /etc/bash.bashrc
. ~/.config/shell/aliases.sh
. ~/.config/shell/bash-preexec.sh
. ~/.config/shell/functions.sh
. ~/.config/shell/prompt.sh

# Set the prompt
if [ "$TERM" != "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
	__setup_prompt "\h" "\w" "\\$"
	__timer_reset;
	__prompt_posthook() { history -a; }
	preexec_functions+=(__timer_start)
	precmd_functions+=(__draw_prompt)
fi

# shell optional behavior
shopt -s autocd
shopt -s extglob

# history
shopt -s cmdhist
shopt -s histappend
shopt -s lithist
declare -r HISTCONTROL=ignoreboth:erasedups
HISTTIMEFORMAT="%F %T %z "
HISTSIZE=
HISTFILESIZE=

# Bash preexec overrides HISTCONTROL by default
__bp_adjust_histcontrol() { :; }

# Keybindings
if [ "$TERM" != "dumb" ] && command -v fzf >/dev/null; then
	__try_source ~/.nix-profile/share/fzf/key-bindings.bash ||
		. ~/.config/shell/fzf/key-bindings.bash
fi

# bash completion
export COMP_KNOWN_HOSTS_WITH_HOSTFILE=""
if declare -F _completion_loader >/dev/null; then
	if [ "$TERM" != "dumb" ] && command -v fzf >/dev/null; then
		__try_source ~/.nix-profile/share/fzf/completion.bash ||
			. ~/.config/shell/fzf/completion.bash
	fi
	if declare -F __git_complete __git_main >/dev/null; then
		__git_complete netsettings __git_main
	fi
fi

# Disable the beep
if command -v xset >/dev/null && [ -n "${DISPLAY}" ]; then
	xset -b &>/dev/null;
fi

# inline direnv hook to avoid calling direnv during shell startup
# generated by "direnv hook bash"
# added timeout
if command -v direnv >/dev/null; then
	. ~/.config/shell/direnv.bash
fi

# Bookmark paths
export CDPATH=":$XDG_DATA_HOME/cdpath${CDPATH:+:$CDPATH}"
mark() {
	mkdir -p "$XDG_DATA_HOME/cdpath"
	ln -sv "$PWD" "$XDG_DATA_HOME/cdpath/${1:-@${PWD##*/}}"
}

__try_source ~/.bashrc.local || true
unset __try_source
