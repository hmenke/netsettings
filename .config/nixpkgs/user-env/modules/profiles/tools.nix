{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    aspell' = aspellWithDicts (dicts: with dicts; [ de en ]);
    diffoscope' = diffoscope.override { enableBloat = false; };
    pass-wayland' = pass-wayland.withExtensions (ext: with ext; [ pass-otp ]);
  in [
    age
    aspell'
    bat
    bfs
    binutils
    bubblewrap
    cachix
    delta
    diffoscope'
    direnv
    dive
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
    gobuster
    gocryptfs
    gvfs
    hdf5.bin
    httm
    hyperfine
    isync
    lemonade
    libarchive
    lsof
    mg
    msmtp
    mtr
    ncdu
    neomutt
    neovim
    nix-direnv
    nix-index
    nix-output-monitor
    nixpkgs-fmt
    nmap
    openssl
    pandoc
    par2cmdline
    pass-git-helper
    pass-wayland'
    patchelf
    progress
    pv
    python3
    qpdf
    rclone
    restic
    ripgrep
    ripgrep-all
    ruff
    rustic
    sccache
    sops
    sqlite
    sshpass
    sshuttle
    subfinder
    syncthing
    tailspin
    taskspooler
    tree
    unison
    upterm
    uv
    yt-dlp
  ];
}
