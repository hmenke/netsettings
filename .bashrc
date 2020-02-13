# Set the prompt
. ~/.config/shell/prompt.sh
__setup_prompt "\h" "\w" "\\$"
__timer_reset;
__prompt_posthook() { history -a; }
trap '__timer_start' DEBUG
PROMPT_COMMAND="${PROMPT_COMMAND:+"${PROMPT_COMMAND%;}; "}__draw_prompt;"

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
    . ~/.config/shell/fzf/key-bindings.bash
fi

# bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
if command -v fzf > /dev/null; then
    . ~/.config/shell/fzf/completion.bash
fi

# Disable the beep
if [ -n "${DISPLAY}" ]; then
    xset -b;
fi

# Source common configuration
. ~/.config/shell/aliases.sh
. ~/.config/shell/environment.sh
. ~/.config/shell/functions.sh
. ~/.config/shell/z.sh
