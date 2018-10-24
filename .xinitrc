setxkbmap us altgr-intl

if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

awesome
