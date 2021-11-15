# Source environment variables
. ~/.config/shell/environment.sh

# Exit if not interactive
case $- in
	*i*) ;;
	*) return ;;
esac

# Source common configuration
. ~/.config/shell/aliases.sh
. ~/.config/shell/functions.sh
. ~/.config/shell/prompt.sh

# Set the prompt
if [ "$TERM" != "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
	__setup_prompt "\h" "\w" "\\$"
	__timer_reset;
	__prompt_posthook() { history -a; }
	trap '__timer_start "$_"' DEBUG
	PROMPT_COMMAND="${PROMPT_COMMAND//__vte_prompt_command/__vte_prompt_command_}"
	PROMPT_COMMAND="${PROMPT_COMMAND+${PROMPT_COMMAND//;__draw_prompt/};}__draw_prompt;"
fi

# shell optional behavior
shopt -s autocd
shopt -s extglob

# history
shopt -s cmdhist
shopt -s histappend
shopt -s lithist
HISTCONTROL=ignoreboth:erasedups
HISTTIMEFORMAT="%F %T %z "
HISTFILE=~/.cache/bash_history
HISTSIZE=
HISTFILESIZE=

# Keybindings
bind '"\e[A": history-search-backward' &>/dev/null
bind '"\e[B": history-search-forward' &>/dev/null
bind '"\ew": copy-region-as-kill' &>/dev/null
bind '"\C-w": kill-region' &>/dev/null
bind '"\eq": kill-whole-line' &>/dev/null
if [ "$TERM" != "dumb" ] && command -v fzf > /dev/null; then
	. ~/.config/shell/fzf/key-bindings.bash
fi

# bash completion
export COMP_KNOWN_HOSTS_WITH_HOSTFILE=""
if declare -f _completion_loader &>/dev/null; then
	if [ "$TERM" != "dumb" ] && command -v fzf > /dev/null; then
		. ~/.config/shell/fzf/completion.bash
	fi
	_completion_loader git
	if type __git_wrap__git_main &>/dev/null; then
		complete -o default -o nospace -F __git_wrap__git_main netsettings
	elif type _git &>/dev/null; then
		complete -o default -o nospace -F _git netsettings
	fi
fi

# Disable the beep
if command -v xset > /dev/null && [ -n "${DISPLAY}" ]; then
	xset -b 2>/dev/null;
fi

# direnv
if command -v direnv > /dev/null; then
	eval "$(direnv hook bash | sed 's!"/nix/store/[^/]*/bin/direnv"!direnv!g')"
fi

if [ -f ~/.bashrc.local ]; then
	. ~/.bashrc.local
fi
