#!/bin/sh

# Reimplementation of
# https://github.com/yousufinternet/config-files/blob/master/.config/bspwm/scripts/swallow.py

if ! command -v xprop >/dev/null; then
	echo "Window swallowing requires xprop"
	return 1 2>/dev/null || exit 1
fi

# Excludes are simply stored in a file
mkdir -p ~/.cache/bspwm
add_exclude() {
	if ! grep -Fiq "$1" ~/.cache/bspwm/swallow_excludes; then
		echo "$1" >> ~/.cache/bspwm/swallow_excludes
	fi
}

add_exclude "firefox"
add_exclude "xterm"

bspc subscribe all | while read -r line; do
	case $line in
		node_add*)
			new_wid="${line##* }"
			last_wid="$(bspc query -N -d -n last.window)"

			# Don't replace floating or fullscreen
			if [ -n "$(bspc query -N -n "$new_wid".floating)" ] ||
				[ -n "$(bspc query -N -n "$new_wid".fullscreen)" ] ||
				[ -n "$(bspc query -N -n "$last_wid".floating)" ] ||
				[ -n "$(bspc query -N -n "$last_wid".fullscreen)" ]; then
				continue
			fi

			# Check the excludes
			while read -r name; do
				if xprop -id "$new_wid" WM_CLASS | grep -Fiq "$name"; then
					continue 2 # continue the outer loop
				fi
			done < ~/.cache/bspwm/swallow_excludes

			# Get PIDs of the windows
			new_pid="$(xprop -id "$new_wid" _NET_WM_PID | grep -o '[0-9]*')"
			last_pid="$(xprop -id "$last_wid" _NET_WM_PID | grep -o '[0-9]*')"
			if [ -z "$new_pid" ] || [ -z "$last_pid" ]; then
				continue
			fi

			# Check if the new process is a child of the last
			if pstree -T -p "$last_pid" | grep -Fq "$new_pid"; then
				bspc node --swap "$last_wid" --follow
				bspc node "$new_wid" --flag private=on
				bspc node "$last_wid" --flag hidden=on
				bspc node "$last_wid" --flag private=on
				swallowed="$new_wid $last_wid\n$swallowed"
			fi
			;;
		node_remove*)
			removed_wid="${line##* }"
			swallowed_line="$(echo "$swallowed" | grep -F "$removed_wid")"

			# Restore the swallowed window and remove it from the list
			if [ -n "$swallowed_line" ]; then
				swallowed_wid="${swallowed_line##* }"
				bspc node "$swallowed_wid" --flag hidden=off
				bspc node --focus "$swallowed_wid"
				swallowed="$(echo "$swallowed" | grep -Fv "$removed_wid")"
			fi
			;;
	esac
done
