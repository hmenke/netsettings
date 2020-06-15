#!/bin/sh

# Reimplementation of
# https://github.com/yousufinternet/config-files/blob/master/.config/bspwm/scripts/swallow.py

if ! command -v xprop >/dev/null; then
	echo "Window swallowing requires xprop"
	return 1 2>/dev/null || exit 1
fi

if ! command -v pstree >/dev/null; then
	echo "Window swallowing requires pstree"
	return 1 2>/dev/null || exit 1
fi

if pgrep -f -x "bspc subscribe node_add" >/dev/null; then
	echo "Someone else is already subscribed to node_add"
	return 1 2>/dev/null || exit 1
fi

if pgrep -f -x "bspc subscribe node_remove" >/dev/null; then
	echo "Someone else is already subscribed to node_remove"
	return 1 2>/dev/null || exit 1
fi

# Excludes and current state are kept in normal files
cachedir="${XDG_CACHE_HOME:-${HOME}/.cache}/bspwm"
mkdir -p "${cachedir}"
excludes_file="${cachedir}/swallow_excludes"
state_file="${cachedir}/swallow_state"
lock_file="${cachedir}/swallow_lock"

add_exclude() {
	if ! grep -Fiq "$1" "${excludes_file}"; then
		echo "$1" >> "${excludes_file}"
	fi
}

# My default excludes
add_exclude "firefox"
add_exclude "xterm"

bspc subscribe node_add | while read -r line; do
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
	done < "${excludes_file}"

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
		{
			flock -x 9
			echo "$new_wid $last_wid" >> "${state_file}";
		} 9> "${lock_file}"
	fi
done &

bspc subscribe node_remove | while read -r line; do
	removed_wid="${line##* }"
	swallowed_line="$(grep -F "$removed_wid" "${state_file}")"

	# Restore the swallowed window and remove it from the list
	if [ -n "$swallowed_line" ]; then
		swallowed_wid="${swallowed_line##* }"
		bspc node "$swallowed_wid" --flag hidden=off
		bspc node --focus "$swallowed_wid"
		{
			flock -x 9
			tmp_file="$(mktemp)"
			grep -Fv "$removed_wid" "${state_file}" > "${tmp_file}"
			mv "${tmp_file}" "${state_file}"
		} 9> "${lock_file}"
	fi
done &
