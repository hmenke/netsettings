{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ de en ]))
    cachix
    delta
    direnv
    emacsPgtkGcc
    file
    fzf
    gh
    git-annex
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
    pre-commit
    python3
    python3Packages.black
    qpdf
    rclone
    sshpass
    sshuttle
    syncthing
    tree
    ts
    unison
    unzip
    youtube-dl
    zip
  ];
}
