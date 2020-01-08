# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    if [ -r ~/.dircolors ]; then
        eval "$(dircolors -b ~/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
    alias ls="ls -F --color=auto"
    alias grep="grep --color=auto"
    alias fgrep="fgrep --color=auto"
    alias egrep="egrep --color=auto"
fi
alias k="ls -l --hide='*~' --group-directories-first"
alias l="ls -la --group-directories-first"
alias la="ls -a --group-directories-first"
alias ll="ls -l --group-directories-first"
alias lla="ls -la --group-directories-first"

alias ..="cd .."
alias ...="cd ../.."

alias rm="rm -I"

# Printing aliases
if command -v lp > /dev/null; then
    alias lp-fit="lp -o media=a4 -o sides=two-sided-long-edge -o fitplot -o collate=true"
    alias lp-fit_s="lp -o media=a4 -o sides=two-sided-short-edge -o fitplot -o collate=true"
fi

# TeXLive
alias latexdef="texdef -t latex"
alias setuptex="source /opt/context/tex/setuptex"
alias setuplmtx='export PATH=/opt/context-lmtx/tex/texmf-linux-64/bin${PATH:+:${PATH}}'
alias ctxdef="mtxrun --silent --path=/tmp --script context --extra=meaning --once  --noconsole --nostatistics"

if command -v xclip > /dev/null; then
    alias xclipboard="xclip -selection clipboard"
fi

# https://sgeb.io/posts/2016/11/til-git-diff-anywhere/
if command -v git > /dev/null; then
    alias diff="git --no-pager diff --color=auto --no-ext-diff --no-index"
fi

# GPG sign
alias gpgsign="gpg --armor --output '-' --detach-sig"
