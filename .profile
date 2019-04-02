# Add ~/.bin to $PATH
if ! echo ${PATH} | grep -E -q "(^|:)${HOME}/.bin($|:)"; then
    export PATH=${HOME}/.bin${PATH:+:${PATH}}
fi

# TeXLive
export PDFVIEWER=xdg-open

if command -v vim > /dev/null; then
    export EDITOR=vim
fi
