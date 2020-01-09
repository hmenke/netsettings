# Configure z
_Z_CMD=_z_no_fzf
_Z_DATA="${HOME}/.cache/z"
touch "${_Z_DATA}"

source ~/.config/shell/z/z.sh

_z_fzf() {
    [ $# -gt 0 ] && _z "$*" && return
    cd "$(_z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')"
}

# Wrap with fzf if available
if command -v fzf > /dev/null; then
    alias z='_z_fzf'
else
    alias z='_z_no_fzf'
fi
