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
setopt autocd
setopt extendedglob
setopt nomatch
setopt notify
setopt shwordsplit

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
