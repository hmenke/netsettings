# enable color support of ls and also add handy aliases
if command -v dircolors >/dev/null; then
	if [ -r ~/.dircolors ]; then
		eval "$(dircolors -b ~/.dircolors)"
	else
		eval "$(dircolors -b)"
	fi
fi
if 2>&1 ls --version | grep -qF GNU; then
	if [ -n "$INSIDE_EMACS" ]; then
		alias ls="ls -F --color=auto"
	else
		alias ls="ls -F --color=auto --hyperlink=auto"
	fi
	alias l="ls -lh --hide='*~' --group-directories-first"
	alias ll="ls -lah --group-directories-first"
else
	alias ls="ls -FG"
	alias l="ls -lh"
	alias ll="ls -lah"
fi
alias grep="grep --color=auto"

alias cd='cd -P'
alias ..="cd .."
alias ...="cd ../.."

if 2>&1 rm --version | grep -qF GNU; then
	alias rm="rm -I"
fi

# Emacs
alias emacs='emacsclient -a "" -c -t'
alias gemacs='emacsclient -a "" -c'
alias magit='emacsclient -a "" -c -t -e "(progn (magit-status) (delete-other-windows))"'

# Printing aliases
if command -v lp > /dev/null; then
	alias lp-fit="lp -o media=a4 -o sides=two-sided-long-edge -o fitplot -o collate=true"
	alias lp-fit_s="lp -o media=a4 -o sides=two-sided-short-edge -o fitplot -o collate=true"
fi

# TeXLive
alias latexdef="texdef -t latex"
alias setuptex=". /opt/context/tex/setuptex"
alias setuplmtx='export PATH=/opt/context-lmtx/tex/texmf-linux-64/bin${PATH:+:${PATH}}'
alias ctxdef="mtxrun --path=/tmp --script interface --meaning"

if command -v xclip > /dev/null; then
    alias xclipboard="xclip -selection clipboard"
fi

# diff
if diff --help | grep -qFe '--color'; then
	alias diff="diff --color -u"
else
	alias diff="diff -u"
fi

# git grep
if command -v git > /dev/null; then
	alias gg="git grep --heading --break"
fi

# GPG sign
alias gpgsign="gpg --armor --output '-' --detach-sig"

# netsettings
alias netsettings='git --git-dir="$XDG_DATA_HOME/netsettings/git" --work-tree="${HOME}"'

# unison
alias unison="env -u DISPLAY unison -ui text"

# SSH
alias ssh-exit-all='\ls -1q ~/.ssh/control-*.sock 2>/dev/null | xargs -I{} echo "Exiting {}"'

# git-branchless
if command -v git-branchless &>/dev/null; then
	alias git="git-branchless wrap --"
fi
