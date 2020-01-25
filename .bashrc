# Set different color for host on SSH
if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
    HOST_COLOR="\[\e[1;93m\]"
else
    HOST_COLOR="\[\e[1;92m\]"
fi

source ~/.config/shell/prompt.sh

# Set the prompt
__timer_reset;
trap '__timer_start' DEBUG
__draw_prompt() {
    __last_status="$?"
    __timer_stop
    __git_ps1 "╭╴${HOST_COLOR}\h \[\e[1;91m\]\w\[\e[0m\]" "$(__timer_show)$(__show_status)
╰╴\[\e[0;94m\]\\$ \[\e[0m\]"
    __timer_reset
    history -a
}
PROMPT_COMMAND="${PROMPT_COMMAND:+"$PROMPT_COMMAND; "}__draw_prompt;"

# shell optional behavior
shopt -s autocd
shopt -s extglob

# history
shopt -s histappend
HISTCONTROL=ignoreboth:erasedups
HISTFILE=~/.cache/bash_history
HISTSIZE=1000
HISTFILESIZE=1000

# Keybindings
if command -v fzf > /dev/null; then
    source ~/.config/shell/fzf/key-bindings.bash
fi

# bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
if command -v fzf > /dev/null; then
    source ~/.config/shell/fzf/completion.bash
fi

# Disable the beep
if [ -n "${DISPLAY}" ]; then
    xset -b;
fi

# Source common configuration
source ~/.config/shell/aliases.sh
source ~/.config/shell/environment.sh
source ~/.config/shell/functions.sh
source ~/.config/shell/z.sh
