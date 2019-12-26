# Cropping for TeX.SX
function crop {
    pdfcrop "$2" "/tmp/${2%%.pdf}-crop.pdf"
    if command -v mudraw >/dev/null; then
        mudraw -r "$1" -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    else
        mutool draw -r "$1" -o "${2%%.pdf}.png" "/tmp/${2%%.pdf}-crop.pdf"
    fi
}

# Search ConTeXt source tree
function ctxgrep {
    local CTXPATH CTXMODULES
    if [ "$(kpsexpand "\$TEXMFCONTEXT")" = "\$TEXMFCONTEXT" ]; then
        CTXPATH="$(kpsexpand "\$TEXMFDIST")/tex/context"
    else
        CTXPATH="$(kpsexpand "\$TEXMFCONTEXT" 2>/dev/null)/tex/context"
        CTXMODULES="$(kpsexpand "\$TEXMFMODULES" 2>/dev/null)/tex/context"
    fi
    if command -v ag > /dev/null; then
        ag --ignore='*.mkii' --ignore '*.pat' --ignore 'lang-*.lua' --ignore patterns "$@" "${CTXPATH}" "${CTXMODULES}"
    else
        grep -r --exclude={*.mkii,*.pat} --exclude-dir=patterns "$@" "${CTXPATH}" "${CTXMODULES}"
    fi
}

function mpgrep {
    local MPPATH
    if [ "$(kpsexpand "\$TEXMFCONTEXT")" = "\$TEXMFCONTEXT" ]; then
        MPPATH="$(kpsexpand "\$TEXMFDIST")/metapost"
    else
        MPPATH="$(kpsexpand "\$SELFAUTOPARENT")/texmf-context/metapost"
    fi
    if command -v ag > /dev/null; then
        ag "$@" "${MPPATH}"
    else
        grep -r "$@" "${MPPATH}"
    fi
}

# Paste services

function dpaste {
    local URL
    URL=$(curl -s -F "syntax=${1:-text}" -F "expiry_days=${2:-10}" -F "content=<-" http://dpaste.com/api/v2/)
    echo "${URL} (expires in ${2:-10} days)"
}

function termbin {
    nc termbin.com 9999
}

function pwgen {
    </dev/urandom tr -dc "${2:-A-Za-z0-9}" | head -c "${1:-10}"
    echo
}
