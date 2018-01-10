#!/bin/sh

cd "$HOME/.debian"

rm Release Packages

cat << EOF > Release
Origin: My local repo
Label: My local repo
Archive: stretch
Suite: stretch
Codename: stretch
Architectures: amd64
Components: main contrib non-free
Description: My local repo consists all packages that I need but that are not available in Debian
EOF

apt-ftparchive packages . > Packages

apt-ftparchive release . >> Release

gpg2 --clearsign -o InRelease Release

gpg2 -abs -o Release.gpg Release
