{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    aspell' = aspellWithDicts (dicts: with dicts; [ de en ]);
    diffoscope' = diffoscope.override { enableBloat = false; };
    pass-wayland' = pass-wayland.withExtensions (ext: with ext; [ pass-otp ]);
  in [
    aspell'
    bat
    cachix
    delta
    diffoscope'
    direnv
    emacs29-pgtk
    expect
    fd
    ffmpeg
    file
    fq
    fzf
    gh
    ghostscript
    git-crypt
    git-filter-repo
    git-lfs
    gitFull
    glab
    glib
    gnuplot_qt
    gvfs
    isync
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
    pass-wayland'
    pv
    python3
    qpdf
    rclone
    restic
    ripgrep
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
