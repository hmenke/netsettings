{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ de en ]))
    cachix
    delta
    diffoscope
    direnv
    emacsPgtkNativeComp
    file
    ffmpeg
    fzf
    gh
    ghostscript
    git-crypt
    git-filter-repo
    git-lfs
    gitFull
    glib
    gnuplot_qt
    gvfs
    isync
    jq
    lab
    libarchive
    msmtp
    neomutt
    neovim
    nix-direnv
    nix-index
    nixpkgs-fmt
    openssl
    pandoc
    pass-git-helper
    (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
    pv
    python3
    python3Packages.black
    qpdf
    rclone
    sshpass
    sshuttle
    syncthing
    tree
    ts
    unison-bin
    unzip
    yt-dlp
    zip
  ];
}
