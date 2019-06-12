# Add ~/.bin to $PATH
if ! echo ${PATH} | grep -E -q "(^|:)${HOME}/.bin($|:)"; then
    export PATH=${HOME}/.bin${PATH:+:${PATH}}
fi

# TeXLive
export PDFVIEWER=xdg-open

# pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

# Prefer nvim but fall back to vim
if command -v nvim > /dev/null; then
    export EDITOR=nvim
elif command -v vim > /dev/null; then
    export EDITOR=vim
fi
