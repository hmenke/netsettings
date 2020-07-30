#!/bin/sh

launch_polybar() {
	if ! pgrep polybar | xargs -i cat "/proc/{}/environ" 2>/dev/null | grep -qxz "MONITOR=$1"; then
		MONITOR="$1" polybar --reload default &
	fi
}

setup_monitors() {
	for MONITOR in $(bspc query -M --names); do
		bspc monitor $MONITOR -d 1 2 3 4 5 6 7 8 9 0
		launch_polybar "$MONITOR"
	done
}

setup_monitors

pkill -f -x "bspc subscribe monitor_add monitor_geometry"
bspc subscribe monitor_add monitor_geometry | while read -r line; do
        setup_monitors
done &
