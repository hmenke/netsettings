# Source common configuration
. ~/.config/shell/aliases.sh
. ~/.config/shell/environment.sh
. ~/.config/shell/functions.sh
. ~/.config/shell/prompt.sh

# Set the prompt
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
HISTTIMEFORMAT="%F %T %z "
HISTFILE=~/.cache/bash_history
HISTSIZE=10000000
HISTFILESIZE=10000000

# Keybindings
bind '"\e[A": history-search-backward' 2>/dev/null
bind '"\e[B": history-search-forward' 2>/dev/null
if command -v fzf > /dev/null; then
    . ~/.config/shell/fzf/key-bindings.bash
fi

# bash completion
if declare -f _completion_loader 2>&1 >/dev/null; then
    if command -v fzf > /dev/null; then
        . ~/.config/shell/fzf/completion.bash
    fi
    _completion_loader git
    complete -o default -o nospace -F _git netsettings
fi

# Disable the beep
if command -v xset > /dev/null && [ -n "${DISPLAY}" ]; then
    xset -b 2>/dev/null;
fi

# direnv
if command -v direnv > /dev/null; then
    eval "$(direnv hook bash)"
fi
