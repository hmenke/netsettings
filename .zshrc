# Source common configuration
. ~/.config/shell/aliases.sh
. ~/.config/shell/environment.sh
. ~/.config/shell/functions.sh
. ~/.config/shell/prompt.sh

# Set the prompt
__setup_prompt "%M" "%~" "\$"
__timer_reset;
autoload -U add-zsh-hook
add-zsh-hook preexec __timer_start
add-zsh-hook precmd __draw_prompt

# shell optional behavior
setopt autocd
setopt extendedglob
setopt nomatch
setopt notify
setopt shwordsplit
unsetopt hup
unsetopt check_jobs

# history
setopt appendhistory
setopt inc_append_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
HISTFILE=~/.cache/zsh_history
HISTSIZE=1000
SAVEHIST=1000

# Bash-y keybindings
bindkey -e
bindkey "^[[1;3C" forward-word
bindkey "^[[1;5C" forward-word
bindkey "^[[1;3D" backward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[3~" delete-char
autoload -U select-word-style
select-word-style bash
if command -v fzf > /dev/null; then
    . ~/.config/shell/fzf/key-bindings.zsh
fi

# edit command line in editor
autoload -z edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# autocompletion
autoload -Uz compinit
compinit -d ~/.cache/zcompdump
if command -v fzf > /dev/null; then
    . ~/.config/shell/fzf/completion.zsh
fi
compdef '_dispatch git git' netsettings

# Disable the beep
unsetopt beep
if [ -n "${DISPLAY}" ]; then
    xset -b
fi

. ~/.config/shell/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
bindkey '^ ' autosuggest-accept
. ~/.config/shell/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
