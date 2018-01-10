# Add ~/.bin to $PATH
if [ -z $(echo $PATH | grep $HOME/.bin) ]; then
    export PATH=$HOME/.bin:$PATH
fi

# Rust
if [ -z $(echo $PATH | grep $HOME/.cargo/bin) ]; then
    export PATH=$HOME/.cargo/bin:$PATH
fi

# TeXLive
export PDFVIEWER=xdg-open
