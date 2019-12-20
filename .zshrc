# Set colorful prompt
if [ -n "${SSH_CLIENT}" ] || [ -n "${SSH_TTY}" ]; then
    source ~/.profile
    HOST_COLOR="%B%F{3}"
else
    HOST_COLOR="%B%F{2}"
fi
PS1="╭╴${HOST_COLOR}%M %B%F{1}%~%f%b
╰╴%F{12}\$ %f"

# shell optional behavior
setopt appendhistory autocd extendedglob nomatch notify
HISTFILE=~/.cache/zsh_history
SAVEHIST=5000
bindkey -e
bindkey "^[[1;3C" forward-word
bindkey "^[[1;5C" forward-word
bindkey "^[[1;3D" backward-word
bindkey "^[[1;5D" backward-word

# autocompletion
autoload -Uz compinit
compinit

# Disable the beep
unsetopt beep

# Source common configuration
source ~/.config/shell/aliases.sh
source ~/.config/shell/environment.sh
source ~/.config/shell/functions.sh

source ~/.config/shell/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
bindkey '^ ' autosuggest-accept
source ~/.config/shell/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
