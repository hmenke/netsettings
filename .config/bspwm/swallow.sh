#!/bin/sh

# Reimplementation of
# https://github.com/yousufinternet/config-files/blob/master/.config/bspwm/scripts/swallow.py

if ! command -v xprop >/dev/null; then
	echo "Window swallowing requires xprop"
	return 1 2>/dev/null || exit 1
fi

# Excludes and current state are kept in normal files
cachedir="${XDG_CACHE_HOME:-${HOME}/.cache}/bspwm"
excludes_file="${cachedir}/swallow_excludes"
state_file="${cachedir}/swallow_state"
lock_file="${cachedir}/swallow_lock"
mkdir -p "${cachedir}"
touch "${state_file}"
touch "${lock_file}"

add_exclude() {
	if ! grep -Fiq "$1" "${excludes_file}"; then
		echo "$1" >> "${excludes_file}"
	fi
}

# My default excludes
add_exclude "firefox"
add_exclude "xterm"

# Helper function to check if process is a child of another
# Checks if $2 is a child of $1
is_child() {
	children="$(pgrep -P "$1")"
	for child in $children; do
		if [ "$child" = "$2" ]; then
			return 0 # true
		elif is_child "$child" "$2"; then
			return 0 #true
		fi
	done
	return 1 # false
}

pkill -f -x "bspc subscribe node_add"
bspc subscribe node_add | while read -r line; do
	new_wid="${line##* }"
	last_wid="$(bspc query -N -d -n last.window)"

	# If the window IDs are empty or the same for whatever reason skip them
	if [ -z "$new_wid" ] || [ -z "$last_wid" ] || [ "$new_wid" = "$last_wid" ]; then
		continue
	fi

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
	done < "${excludes_file}"

	# Get PIDs of the windows.  If the PID is the same, it's probably a
	# program that wants to spawn a separate window.  Better not swallow
	# that.
	new_pid="$(xprop -id "$new_wid" _NET_WM_PID | grep -o '[0-9]*')"
	last_pid="$(xprop -id "$last_wid" _NET_WM_PID | grep -o '[0-9]*')"
	if [ -z "$new_pid" ] || [ -z "$last_pid" ] || [ "$new_pid" = "$last_pid" ]; then
		continue
	fi

	# Check if the new process is a child of the last
	if is_child "$last_pid" "$new_pid"; then
		bspc node --swap "$last_wid" --follow
		#bspc node "$new_wid" --flag private=on
		bspc node "$last_wid" --flag hidden=on
		#bspc node "$last_wid" --flag private=on
		{
			flock -x 9
			echo "$new_wid $last_wid" >> "${state_file}";
		} 9> "${lock_file}"
	fi
done &

pkill -f -x "bspc subscribe node_remove"
bspc subscribe node_remove | while read -r line; do
	removed_wid="${line##* }"
	swallowed_line="$(grep -F "$removed_wid" "${state_file}")"

	# Restore the swallowed window and remove it from the list
	if [ -n "$swallowed_line" ]; then
		swallowed_wid="${swallowed_line##* }"
		bspc node "$swallowed_wid" --flag hidden=off
		#bspc node "$swallowed_wid" --flag private=off
		bspc node --focus "$swallowed_wid"
		{
			flock -x 9
			tmp_file="$(mktemp)"
			grep -Fv "$removed_wid" "${state_file}" > "${tmp_file}"
			mv "${tmp_file}" "${state_file}"
		} 9> "${lock_file}"
	fi
done &
