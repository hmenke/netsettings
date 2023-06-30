{ config, lib, pkgs, ... }:

let
  unison-bin = with pkgs;
    stdenv.mkDerivation rec {
      pname = "unison-bin";
      version = "2.53.0";
      ocamlVersion = "4.10.2";
      name = "${pname}-${version}+ocaml-${ocamlVersion}";

      src = fetchurl {
        url = "https://github.com/bcpierce00/unison/releases/download/v${version}/unison-v${version}+ocaml-${ocamlVersion}+x86_64.linux.tar.gz";
        sha256 = "sha256-iWAt/Y813ZC2zoaT7rezJYNFBkea/smz0UJx7cMl/T0=";
      };
      sourceRoot = ".";

      nativeBuildInputs = [ autoPatchelfHook ];
      buildInputs = [ gtk3 pango ];

      dontConfigure = true;
      dontBuild = true;

      outputs = [ "out" "doc" ];

      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin $doc/share/${pname}
        cp -r bin/* $out/bin/
        cp unison-manual.* $doc/share/${pname}/
        runHook postInstall
      '';
    };
in
{
  userPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ de en ]))
    cachix
    delta
    (diffoscope.override { enableBloat = false; })
    direnv
    emacs29-pgtk
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
