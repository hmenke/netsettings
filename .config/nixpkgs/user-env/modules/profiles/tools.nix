{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ de en ]))
    cachix
    delta
    (diffoscope.override { enableBloat = false; })
    direnv
    emacs29-pgtk
    expect
    file
    ffmpeg
    fq
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
    lab
    lemonade
    libarchive
    msmtp
    neomutt
    neovim
    nix-direnv
    nix-index
    nix-output-monitor
    nixpkgs-fmt
    openssl
    pandoc
    pass-git-helper
    (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
    pv
    python3
    qpdf
    rclone
    restic
    ruff
    sshpass
    sshuttle
    syncthing
    tree
    ts
    unison
    yt-dlp
  ];
}
