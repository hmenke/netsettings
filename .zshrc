# Set different color for host on SSH
if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
    HOST_COLOR="%B%F{3}"
else
    HOST_COLOR="%B%F{2}"
fi

source ~/.config/shell/prompt.sh

# Set the prompt
autoload -U add-zsh-hook
__timer_reset;
add-zsh-hook preexec __timer_start
__draw_prompt() {
    __last_status="$?"
    __timer_stop
    __git_ps1 "╭╴${HOST_COLOR}%M %B%F{1}%~%f%b" "$(__timer_show)$(__show_status)
╰╴%F{12}\$ %f"
    __timer_reset
}
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
autoload -U select-word-style
select-word-style bash
if [ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]; then
    source /usr/share/doc/fzf/examples/key-bindings.zsh
fi

# edit command line in editor
autoload -z edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# autocompletion
autoload -Uz compinit
compinit
if [ -f /usr/share/zsh/vendor-completions/_fzf ]; then
    source /usr/share/zsh/vendor-completions/_fzf
fi

# Disable the beep
unsetopt beep

# Source common configuration
source ~/.config/shell/aliases.sh
source ~/.config/shell/environment.sh
source ~/.config/shell/functions.sh
source ~/.config/shell/z.sh

source ~/.config/shell/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
bindkey '^ ' autosuggest-accept
source ~/.config/shell/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
