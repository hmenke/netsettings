#!/bin/sh

if pgrep -f -x "bspc subscribe monitor_add monitor_geometry" >/dev/null; then
	echo "Someone else is already subscribed to monitor_add and monitor_geometry"
	return 1 2>/dev/null || exit 1
fi

setup_monitors() {
    killall -q polybar
    for MONITOR in $(bspc query -M --names); do
        export MONITOR
        bspc monitor $MONITOR -d 1 2 3 4 5 6 7 8 9 0
        polybar --reload default &
    done
}

setup_monitors

bspc subscribe monitor_add monitor_geometry | while read -r line; do
        setup_monitors
done &

