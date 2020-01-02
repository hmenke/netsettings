# Set different color for host on SSH
if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
    HOST_COLOR="\[\e[1;33m\]"
else
    HOST_COLOR="\[\e[1;32m\]"
fi

source ~/.config/shell/prompt.sh

# Set the prompt
__timer_reset;
trap '__timer_start' DEBUG
PROMPT_COMMAND+='__last_status=$?;'
PROMPT_COMMAND+='__timer_stop;'
PROMPT_COMMAND+='__git_ps1 "╭╴${HOST_COLOR}\h \[\e[1;31m\]\w\[\e[0m\]" "$(__timer_show)$(__show_status)\n╰╴\[\e[0;94m\]\\$ \[\e[0m\]";'
PROMPT_COMMAND+='__timer_reset;'

# shell optional behavior
shopt -s autocd
shopt -s extglob

# history
shopt -s histappend
HISTFILE=~/.cache/bash_history
HISTSIZE=1000
HISTFILESIZE=1000

# Keybindings
if [ -f /usr/share/doc/fzf/examples/key-bindings.bash ]; then
    source /usr/share/doc/fzf/examples/key-bindings.bash
fi

# bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
if [ -f /usr/share/bash-completion/completions/fzf ]; then
    source /usr/share/bash-completion/completions/fzf
fi

# Disable the beep
if [ -n "${DISPLAY}" ]; then
    xset -b;
fi

# Source common configuration
source ~/.config/shell/aliases.sh
source ~/.config/shell/environment.sh
source ~/.config/shell/functions.sh
