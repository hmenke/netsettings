pkgname=netsettings
pkgver=0
pkgrel=1
pkgdesc='Bootstrap all my favourite programs'
arch=('any')
license=('GPL')
install=netsettings.install
backup=('etc/pacman.conf')

makedepends=('grep' 'git')
pkgver() {
    git rev-list --count HEAD
}

# Kernel independent packages
depends=(
    # Tools and libraries
    'apparmor'
    'bash-completion'
    'boost'
    'clang'
    'cmake'
    'dash'
    'debootstrap'
    'dpkg'
    'emacs'
    'etckeeper'
    'firejail'
    'fzf'
    'gdb'
    'git'
    'gnuplot'
    'gsl'
    'jq'
    'lua'
    'lua-lpeg'
    'lvm2'
    'man-db'
    'man-pages'
    'mlocate'
    'neovim'
    'networkmanager'
    'newsboat'
    'nftables'
    'openssh'
    'pacman-contrib'
    'partclone'
    'pass'
    'pass-otp'
    'python-numpy'
    'python-scipy'
    'python-requests'
    'python-xdg'
    'rcs'
    'ripgrep'
    'rsync'
    'shadowsocks-libev'
    'shadowsocks-v2ray-plugin'
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
    'gnome-disk-utility'
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
}
