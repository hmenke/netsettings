# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

let

  config = import ./config.nix;
  overlays = import ./overlays.nix;
  nixexprs = fetchTarball {
    url = "https://nixos.org/channels/nixos-20.03/nixexprs.tar.xz";
  };
  pkgs = import nixexprs { inherit config overlays; };

in

{ name ? "user-env" }:

with pkgs;

buildEnv {
  inherit name;
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths = [
    # command line utils
    aspell
    aspellDicts.de
    aspellDicts.en
    cachix
    direnv
    ffmpeg
    fzf
    (hiPrio gcc)
    gnuplotGit
    isync
    llvmPackages_latest.clang
    msmtp
    neomutt
    newsboat
    niv
    nix-direnv
    p7zip
    pass-otp
    qpdf
    shadowsocks-libev
    ts
    unison
    unzip
    #v2ray-plugin
    valgrind
    youtube-dl
    zip
  ] ++ [
    # GUI
    arandr
    clementine
    dunst
    evince
    feh
    firefox
    gnome3.adwaita-icon-theme
    libnotify
    networkmanagerapplet
    pavucontrol
    polybarFull
    rofi
    scrot
    vlc
    xclip
    xorg.setxkbmap
    xorg.xbacklight
    xorg.xmodmap
    xorg.xsetroot
    xscreensaver
  ] ++ [
    # Proprietary
    dropbox
    google-chrome
    mathematica
    skypeforlinux
    softmaker-office
    zoom-us
  ] ++
  # Local package specifications (if they exist)
  (if builtins.pathExists ./local.nix then import ./local.nix pkgs else []) ++
  # Helpers for declarative package management
  [
    # Script to rebuild the environment from this file.
    (writeScriptBin "nix-rebuild" ''
      #!${runtimeShell}
      ${nix}/bin/nix-env --set -f ~/.config/nixpkgs/buildEnv.nix --argstr name "$(whoami)-user-env-$(date -I)" "$@"
    '')

    # Manifest to make sure imperative nix-env doesn't work (otherwise it will
    # overwrite the profile, removing all packages other than the
    # newly-installed one).
    (writeTextFile {
      name = "break-nix-env-manifest";
      destination = "/manifest.nix";
      text = ''
        throw ''\''
          Your user environment is a buildEnv which is incompatible with
          nix-env's built-in env builder. Edit your home expression and run
          nix-rebuild instead!
        ''\''
      '';
    })

    # To allow easily seeing which nixpkgs version the profile was built from,
    # place the version string in ~/.nix-profile/nixpkgs-version
    (writeTextFile {
      name = "nixpkgs-version";
      destination = "/nixpkgs-version";
      text = lib.version;
    })
  ];
}
