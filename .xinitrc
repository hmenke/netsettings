#!/bin/sh

if [ -f /etx/X11/xinit/.Xresources ]; then
    xrdb -merge /etx/X11/xinit/.Xresources
fi

if [ -f /etx/X11/xinit/.Xmodmap ]; then
    xmodmap /etx/X11/xinit/.Xmodmap
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        if [ -x "$f" ]; then
            . "$f"
        fi
    done
    unset f
fi

if [ -f ~/.Xresources ]; then
    xrdb -merge ~/.Xresources
fi

if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

exec bspwm
