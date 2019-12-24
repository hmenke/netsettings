#!/bin/bash

set -e

GITROOT="$(git rev-parse --show-toplevel)"
if [ "${GITROOT}" = "${HOME}" ]; then
    echo "Can't link config files in place"
    exit 1
fi

echo "Installing config files from ${GITROOT} into ${HOME}"

for file in $(git ls-tree --name-only -r HEAD); do
    case "${file}" in
        ".install.sh"|".gitmodules")
            echo "Skipping file ${file}"
            continue
    esac

    dirname="$(dirname "${file}")"
    mkdir -p "${HOME}/${dirname}";

    relpath="$(realpath --relative-to="${HOME}/${dirname}" "${GITROOT}/${file}")"

    echo "Linking ${file} to ${relpath}"
    ln -s -n -f "${relpath}" "${HOME}/${file}"
done
