# Set colorful prompt
if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
    source ~/.profile
    HOST_COLOR="\[\e[1;33m\]"
else
    HOST_COLOR="\[\e[1;32m\]"
fi
PS1="╭╴${HOST_COLOR}\h \[\e[1;31m\]\w\[\e[0m\]\n╰╴\[\e[0;94m\]\\$ \[\e[0m\]"

# shell optional behavior
shopt -s autocd
shopt -s extglob

# history
shopt -s histappend
HISTFILE=~/.cache/bash_history
HISTSIZE=1000
HISTFILESIZE=1000

# bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Disable the beep
if [ -n "${DISPLAY}" ]; then
    xset -b;
fi

# Source common configuration
source ~/.config/shell/aliases.sh
source ~/.config/shell/environment.sh
source ~/.config/shell/functions.sh
