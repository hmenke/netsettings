# Set colorful prompt
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    HOST_COLOR="\[\e[1;33m\]"
else
    HOST_COLOR="\[\e[1;32m\]"
fi
PS1="╭╴$HOST_COLOR\h \[\e[1;31m\]\w\[\e[0m\]\n╰╴\[\e[0;94m\]\$ \[\e[0m\]"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -F --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
alias k='ls -l --hide=*~ --group-directories-first'
alias l='ls -la --group-directories-first'
alias la='ls -a --group-directories-first'
alias ll='ls -l --group-directories-first'
alias lla='ls -la --group-directories-first'

alias ..='cd ..'
alias ...='cd ../..'

alias rm='rm -I'

# Printing aliases
alias lp-fit='lp -o media=a4 -o sides=two-sided-long-edge -o fitplot -o collate=true'
alias lp-fit_s='lp -o media=a4 -o sides=two-sided-short-edge -o fitplot -o collate=true'

# Python
alias pylab='ipython --pylab'
alias pg='pygmentize'

# TeXLive
alias latexdef='texdef -t latex'
alias setuptex='source /opt/context/tex/setuptex'
alias ctxdef='mtxrun --silent --script context --extra=meaning --once  --noconsole --nostatistics'

# bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Disable the beep
if [ -n "$DISPLAY" ]; then
    xset -b;
fi

# GPG TTY
export GPG_TTY=`tty`

# Shut up GTK!
export NO_AT_BRIDGE=1

# Cropping for TeX.SX
function crop {
    pdfcrop "$2" "/tmp/${2%%.pdf}-crop.pdf"
    if command -v mudraw >/dev/null; then
        mudraw -r $1 -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    else
        mutool draw -r $1 -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    fi
}

# Search ConTeXt source tree
function ctxgrep {
    if [ "$(kpsexpand '$TEXMFCONTEXT')" = "\$TEXMFCONTEXT" ]; then
        CTXPATH="$(kpsexpand '$TEXMFDIST')/tex/context"
    else
        CTXPATH="$(kpsexpand '$SELFAUTOPARENT')"
    fi
    case $1 in
        "mkii")
            shift;
            grep -r --include=*.mkii "$@" $CTXPATH
            ;;
        "mkiv")
            shift;
            grep -r --include=*.mkiv "$@" $CTXPATH
            ;;
        "mkvi")
            shift;
            grep -r --include=*.mkvi "$@" $CTXPATH
            ;;
        "lua")
            shift;
            grep -r --include=*.lua "$@" $CTXPATH
            ;;
        *)
            grep -r --exclude={*.mkii,*.pat} "$@" $CTXPATH
            ;;
    esac
}
