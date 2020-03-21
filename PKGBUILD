pkgname=netsettings
pkgver=0
pkgrel=1
pkgdesc='Bootstrap all my favourite programs'
arch=('any')
license=('GPL')
install=netsettings.install
provides=('texlive-bin' $(pacman -Sgq texlive-most texlive-lang) 'vi' 'vim' 'vim-runtime')

makedepends=('grep' 'git')
pkgver() {
    git rev-list --count HEAD
}

# Kernel independent packages
depends=(
    # Tools and libraries
    'android-tools'
    'android-udev'
    'apparmor'
    'arch-audit'
    'arch-wiki-docs'
    'bash-completion'
    'bc'
    'boost'
    'clang'
    'cmake'
    'dash'
    'debootstrap'
    'dpkg'
    'emacs'
    'etckeeper'
    'ffmpeg'
    'firejail'
    'fzf'
    'gdb'
    'git'
    'gnuplot'
    'go'
    'gsl'
    'jq'
    'lua'
    'lua-lpeg'
    'lvm2'
    'man-db'
    'man-pages'
    'mlocate'
    'mupdf-tools'
    'neomutt'
    'neovim'
    'networkmanager'
    'newsboat'
    'nftables'
    'notmuch'
    'offlineimap'
    'openssh'
    'p7zip'
    'pacman-contrib'
    'partclone'
    'parted'
    'pass'
    'pass-otp'
    'perl-image-exiftool'
    'proxychains-ng'
    'python-matplotlib'
    'python-numpy'
    'python-pynvim'
    'python-requests'
    'python-scipy'
    'python-xdg'
    'qpdf'
    'rcs'
    'ripgrep'
    'rsync'
    'shadowsocks-libev'
    'shadowsocks-v2ray-plugin'
    'sshuttle'
    'sudo'
    'texinfo'
    'tlp'
    'tmux'
    'unison'
    'unzip'
    'valgrind'
    'wget'
    'which'
    'wireguard-tools'
    'youtube-dl'
    'zip'
    'zsh'
    'zsh-completions'

    # GUI
    'arandr'
    'bspwm'
    'caja'
    'caja-open-terminal'
    'clementine'
    'dmenu'
    'dunst'
    'evince'
    'feh'
    'firefox'
    'gimp'
    'gst-plugins-bad'
    'gst-plugins-base'
    'gst-plugins-good'
    'gst-plugins-ugly'
    'lightdm'
    'lightdm-gtk-greeter'
    'mate-settings-daemon'
    'mate-terminal'
    'network-manager-applet'
    'noto-fonts-cjk'
    'noto-fonts-emoji'
    'noto-fonts-extra'
    'pavucontrol'
    'pulseaudio'
    'qemu'
    'redshift'
    'scrot'
    'sxhkd'
    'thunderbird'
    'tk'
    'ttf-dejavu'
    'vlc'
    'xclip'
    'xf86-video-intel'
    'xorg-xbacklight'
    'xorg-xev'
    'xorg-xhost'
    'xorg-xinit'
    'xorg-xset'
    'xscreensaver'
    'xsel'
    'xterm'
    'zathura'
    'zathura-pdf-mupdf'
)

package() {
    cd ..
    for file in $(git ls-files | grep -v -e '^netsettings' -e 'PKGBUILD'); do
        echo "Adding $file"
        install -Dm644 "$file" "$pkgdir/$file"
    done

    install -dm755 "$pkgdir/usr/bin"
    ln -s nvim "$pkgdir/usr/bin/vi"
    ln -s nvim "$pkgdir/usr/bin/vim"
}