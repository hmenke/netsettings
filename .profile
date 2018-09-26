# Add ~/.bin to $PATH
if [ -z $(echo $PATH | grep $HOME/.bin) ]; then
    export PATH=$HOME/.bin:$PATH
fi

# TeXLive
export PDFVIEWER=xdg-open

if command -v vim > /dev/null; then
    export EDITOR=vim
fi
