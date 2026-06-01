# finding files
ff() {
	find . -xdev -iname "$@" -print -o \( -path "*/.*" -o -path "*/*~" \) -prune
}

# interactive diff
ediff() {
	emacsclient -a "" -c -t --eval "(ediff-files \"$1\" \"$2\")"
}

# Cropping for TeX.SX
crop() {
    pdfcrop "$2" "/tmp/${2%%.pdf}-crop.pdf"
    if command -v mudraw >/dev/null; then
        mudraw -r "$1" -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    else
        mutool draw -r "$1" -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    fi
}

# Search ConTeXt source tree
ctxgrep() {
    local CTXPATH CTXMODULES
    if [ -z "$(mtxrun --resolve-path "\$TEXMFCONTEXT")" ]; then
        CTXPATH="$(mtxrun --resolve-path "\$TEXMFDIST")/tex/context"
    else
        CTXPATH="$(mtxrun --resolve-path "\$TEXMFCONTEXT" 2>/dev/null)/tex/context"
        CTXMODULES="$(mtxrun --resolve-path "\$TEXMFMODULES" 2>/dev/null)/tex/context"
    fi
    git -C / grep --color=always --heading --break "$@" -- \
            ":${CTXPATH}/*" \
            ':!*.mkii' \
            ':!*.xml' \
            ':!*.pat' \
            ":(exclude)${CTXPATH#/}/patterns/*" \
        | sed "s+${CTXPATH#/}+/&+g"
    if [ -n "${CTXMODULES}" ]; then
        git -C / grep --color=always --heading --break "$@" -- \
                ":${CTXMODULES}/*" \
                ':!*.mkii' \
                ':!*.xml' \
                ':!*.pat' \
            | sed "s+${CTXMODULES#/}+/&+g"
    fi
}

mpgrep() {
    local MPPATH
    if [ -z "$(mtxrun --resolve-path "\$TEXMFCONTEXT")" ]; then
        MPPATH="$(mtxrun --resolve-path "\$TEXMFDIST")/metapost"
    else
        MPPATH="$(mtxrun --resolve-path "\$SELFAUTOPARENT")/texmf-context/metapost"
    fi
    git -C / grep --color=always --heading --break "$@" -- ":${MPPATH}/*" | sed "s+${MPPATH#/}+/&+g"
}

# Paste services

dpaste() {
    local URL
    URL=$(curl -s -F "syntax=${1:-text}" -F "expiry_days=${2:-10}" -F "content=<-" http://dpaste.com/api/v2/)
    echo "${URL} (expires in ${2:-10} days)"
}

termbin() {
    nc termbin.com 9999
}

pwgen() {
    </dev/urandom tr -dc "${2:-A-Za-z0-9}" | head -c "${1:-10}"
    echo
}

hx() {
    hexdump --color=always -v \
    -e '"%08.8_ax_L[cyan]  "' \
    -e '16/1 "%02x_L[gray:0x00,blue:0x09,blue:0x0a,blue:0x0d,blue:0x20,green:0x21,green:0x22,green:0x23,green:0x24,green:0x25,green:0x26,green:0x27,green:0x28,green:0x29,green:0x2a,green:0x2b,green:0x2c,green:0x2d,green:0x2e,green:0x2f,green:0x30,green:0x31,green:0x32,green:0x33,green:0x34,green:0x35,green:0x36,green:0x37,green:0x38,green:0x39,green:0x3a,green:0x3b,green:0x3c,green:0x3d,green:0x3e,green:0x3f,green:0x40,green:0x41,green:0x42,green:0x43,green:0x44,green:0x45,green:0x46,green:0x47,green:0x48,green:0x49,green:0x4a,green:0x4b,green:0x4c,green:0x4d,green:0x4e,green:0x4f,green:0x50,green:0x51,green:0x52,green:0x53,green:0x54,green:0x55,green:0x56,green:0x57,green:0x58,green:0x59,green:0x5a,green:0x5b,green:0x5c,green:0x5d,green:0x5e,green:0x5f,green:0x60,green:0x61,green:0x62,green:0x63,green:0x64,green:0x65,green:0x66,green:0x67,green:0x68,green:0x69,green:0x6a,green:0x6b,green:0x6c,green:0x6d,green:0x6e,green:0x6f,green:0x70,green:0x71,green:0x72,green:0x73,green:0x74,green:0x75,green:0x76,green:0x77,green:0x78,green:0x79,green:0x7a,green:0x7b,green:0x7c,green:0x7d,green:0x7e,red] "' \
    -e '" |"' \
    -e '16/1 "%_p"' \
    -e '"|\n"' \
    "$@"
}
