# finding files
ff() {
	local left right patt
	left=".*"
	right=".*"
	patt="$1"
	shift
	case $patt in ^*) left="" ;; esac
	case $patt in *$) right="" ;; esac
	find -xdev -iregex "$left$patt$right" "$@" -print -o \( -path "*/.*" -o -path "*/*~" \) -prune
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
    local CTXPATH CTXMODULES SEARCHPATH
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
