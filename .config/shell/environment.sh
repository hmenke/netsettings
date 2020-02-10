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

# make screen use XDG dir
export SCREENRC="${HOME}/.config/screen/screenrc"

# Colors in less (https://github.com/shibumi/hikari-zsh)
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
