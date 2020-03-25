# Add ~/.bin to $PATH
if ! echo "${PATH}" | grep -E -q "(^|:)${HOME}/.local/bin($|:)"; then
    export PATH="${HOME}/.local/bin${PATH:+:${PATH}}"
fi

# TeXLive
if ! echo "${PATH}" | grep -E -q "(^|:)/opt/texlive/2019/bin/x86_64-linux($|:)"; then
    export PATH="/opt/texlive/2019/bin/x86_64-linux${PATH:+:${PATH}}"
fi
export PDFVIEWER=xdg-open

# Prefer nvim but fall back to vim
if command -v nvim > /dev/null; then
    export EDITOR=nvim
elif command -v vim > /dev/null; then
    export EDITOR=vim
fi

# pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

# fzf
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

# Python
export PYTHONSTARTUP="$HOME/.config/cpython/pythonrc"

# GPG TTY
GPG_TTY="$(tty)"
export GPG_TTY
export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"

# Shut up GTK!
export NO_AT_BRIDGE=1

# 256 colors
export TERM=xterm-256color

# XDG base directory support
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"
export ANDROID_SDK_HOME="$XDG_DATA_HOME/share/android"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/notmuchrc"
export UNISON="$XDG_DATA_HOME/unison"

# Colors in less (https://github.com/shibumi/hikari-zsh)
export LESS_TERMCAP_mb="$(printf "\001\033[01;31m\002")"
export LESS_TERMCAP_md="$(printf "\001\033[01;31m\002")"
export LESS_TERMCAP_me="$(printf "\001\033[0m\002")"
export LESS_TERMCAP_se="$(printf "\001\033[0m\002")"
export LESS_TERMCAP_so="$(printf "\001\033[01;44;33m\002")"
export LESS_TERMCAP_ue="$(printf "\001\033[0m\002")"
export LESS_TERMCAP_us="$(printf "\001\033[01;32m\002")"
# Disable less history
export LESSHISTFILE=-
