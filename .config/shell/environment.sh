# Add ~/.bin to $PATH
if ! echo "${PATH}" | grep -E -q "(^|:)${HOME}/.local/bin($|:)"; then
    export PATH="${HOME}/.local/bin${PATH:+:${PATH}}"
fi

# TeXLive
export PDFVIEWER=xdg-open

# Nix
export NIX_INSTALLER_NO_MODIFY_PROFILE=1
if [ -d "/nix" ]; then
	if [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
		. "${HOME}/.nix-profile/etc/profile.d/nix.sh"
	fi
	if [ -e "${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
		. "${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh"
	fi
fi

# Editor
if command -v emacsclient > /dev/null; then
    export EDITOR='emacsclient -c -a "" -t'
elif command -v nvim > /dev/null; then
    export EDITOR=nvim
elif command -v vim > /dev/null; then
    export EDITOR=vim
fi

# ls --time-style
export TIME_STYLE=long-iso

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
unset SSH_ASKPASS

# Shut up GTK!
export NO_AT_BRIDGE=1

# XDG base directory support
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"
export ANDROID_SDK_HOME="$XDG_DATA_HOME/share/android"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/notmuchrc"
export UNISON="$XDG_DATA_HOME/unison"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export MATHEMATICA_USERBASE="$XDG_CONFIG_HOME/Mathematica"

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

# pager
export LESS=FR
export SYSTEMD_LESS=FR
if command -v delta > /dev/null; then
	export GIT_PAGER="delta --light --color-only --max-line-length=4000"
elif command -v diff-highlight > /dev/null; then
	export GIT_PAGER="diff-highlight | less"
else
	export GIT_PAGER="less"
fi

# rclone
export RCLONE_PASSWORD_COMMAND="pass rclone/config"
