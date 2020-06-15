#!/bin/sh

setup_monitors() {
	pkill -f -x "polybar --reload default"
	for MONITOR in $(bspc query -M --names); do
		export MONITOR
		bspc monitor $MONITOR -d 1 2 3 4 5 6 7 8 9 0
		polybar --reload default &
	done
}

setup_monitors

pkill -f -x "bspc subscribe monitor_add monitor_geometry"
bspc subscribe monitor_add monitor_geometry | while read -r line; do
        setup_monitors
done &
