# pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

# GPG TTY
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"

# Shut up GTK!
export NO_AT_BRIDGE=1

# 256 colors
export TERM=xterm-256color

# Limit memory for processes to 80% of total RAM
TOTAL_MEM=$(cat /proc/meminfo | grep -e 'MemTotal:' | grep -oE '([0-9]+)')
ulimit -Sv $[8*${TOTAL_MEM}/10]
