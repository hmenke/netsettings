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

# GPG TTY
GPG_TTY="$(tty)"
export GPG_TTY
export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"

# Shut up GTK!
export NO_AT_BRIDGE=1

# 256 colors
export TERM=xterm-256color

# Limit memory for processes to 80% of total RAM
TOTAL_MEM=$(grep -e 'MemTotal:' /proc/meminfo | grep -oE '([0-9]+)')
ulimit -Sv $((8 * TOTAL_MEM / 10))
