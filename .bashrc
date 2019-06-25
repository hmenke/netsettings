# Set colorful prompt
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    HOST_COLOR="\[\e[1;33m\]"
else
    HOST_COLOR="\[\e[1;32m\]"
fi
PS1="╭╴$HOST_COLOR\h \[\e[1;31m\]\w\[\e[0m\]\n╰╴\[\e[0;94m\]\\$ \[\e[0m\]"

# shell optional behavior
shopt -s autocd

# Limit memory for processes to 80% of total RAM
TOTAL_MEM=$(cat /proc/meminfo | grep -e 'MemTotal:' | grep -oE '([0-9]+)')
ulimit -Sv $[8*${TOTAL_MEM}/10]

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
alias setuplmtx='export PATH=/opt/context-lmtx/tex/texmf-linux-64/bin${PATH:+:${PATH}}'
alias ctxdef='mtxrun --silent --script context --extra=meaning --once  --noconsole --nostatistics'

# https://sgeb.io/posts/2016/11/til-git-diff-anywhere/
if command -v git > /dev/null; then
    alias diff='git --no-pager diff --color=auto --no-ext-diff --no-index'
fi

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

# 256 colors
TERM=xterm-256color

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
        CTXPATH="$(kpsexpand '$TEXMFCONTEXT' 2>/dev/null)/tex/context $(kpsexpand '$TEXMFMODULES' 2>/dev/null)/tex/context"
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
            if command -v ag > /dev/null; then
                ag --ignore='*.mkii' --ignore '*.pat' --ignore 'lang-*.lua' --ignore patterns "$@" $CTXPATH
            else
                grep -r --exclude={*.mkii,*.pat} --exclude-dir=patterns "$@" $CTXPATH
            fi
            ;;
    esac
}

function mpgrep {
    if [ "$(kpsexpand '$TEXMFCONTEXT')" = "\$TEXMFCONTEXT" ]; then
        MPPATH="$(kpsexpand '$TEXMFDIST')/metapost"
    else
        MPPATH="$(kpsexpand '$SELFAUTOPARENT')/texmf-context/metapost"
    fi
    if command -v ag > /dev/null; then
        ag "$@" ${MPPATH}
    else
        grep -r "$@" ${MPPATH}
    fi
}

# Paste services

function dpaste {
    curl -s -F "syntax=${1:text}" -F "content=<-" http://dpaste.com/api/v2/
}

function termbin {
    nc termbin.com 9999
}

function pass_encrypt {
    gpg -e -r F1C5760E45B99A4472E96BFBD65C9AFB4C224DA3 \
        --quiet --yes --compress-algo=none --no-encrypt-to --batch --use-agent
}
