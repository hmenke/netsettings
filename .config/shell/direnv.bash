if timeout -v 0 true >/dev/null 2>&1; then
	__timeout_v() { timeout -v "$@"; }
else
	__timeout_v() { timeout "$@"; }
fi

_direnv_hook() {
	local previous_exit_status=$?
	_direnv_hook() {
		local previous_exit_status=$?
		trap -- '' SIGINT
		eval "$(__timeout_v "${DIRENV_TIMEOUT:-0.5}" direnv export bash)"
		trap - SIGINT
		return $previous_exit_status
	}
	if ! [[ "${PROMPT_COMMAND:-}" =~ _direnv_hook ]]; then
		PROMPT_COMMAND="_direnv_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
	fi
	return $previous_exit_status
}
if ! [[ "${PROMPT_COMMAND:-}" =~ _direnv_hook ]]; then
	PROMPT_COMMAND="_direnv_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi
