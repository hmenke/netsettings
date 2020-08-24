# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

{ name ? "user-env",
  allowUnfree ? true }:

let

  config = { inherit allowUnfree; };
  overlays = import ./overlays.nix;
  nixexprs = fetchTarball {
    url = "https://nixos.org/channels/nixos-20.03/nixexprs.tar.xz";
  };
  pkgs = import nixexprs { inherit config overlays; };

in

with pkgs;

buildEnv rec {
  inherit name;
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths = [
    # command line utils
    aspell
    aspellDicts.de
    aspellDicts.en
    cachix
    direnv
    emacs
    ffmpeg
    fzf
    (hiPrio gcc)
    gnuplotGit
    isync
    llvmPackages_latest.clang
    msmtp
    neomutt
    neovim
    newsboat
    niv
    nix-direnv
    p7zip
    pandoc
    pass-otp
    qpdf
    shadowsocks-libev
    tree
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
  ] ++ lib.lists.optionals allowUnfree [
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

      oldGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
      oldVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

      ${nix}/bin/nix-env --set -f ~/.config/nixpkgs/buildEnv.nix \
      --argstr name "$(whoami)-user-env-$(date -I)" \
      --arg allowUnfree ${lib.trivial.boolToString allowUnfree} \
      "$@"

      newGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
      newVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

      ${diffutils}/bin/diff --color -u \
      --label "generation $oldGeneration" "$oldVersions" \
      --label "generation $newGeneration" "$newVersions" \
      || true
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

    # Since you can't see the versions with nix-env -q anymore, we write them
    # to a file for easy querying
    (let
       collect = pkgs:
         let recurse = x:
           if lib.isDerivation x then [x]
           else if x.recurseForDerivations or false then collect x
           else [];
         in lib.concatMap recurse pkgs;
       versions = map (pkg: pkg.name) (collect paths);
       versionText = lib.strings.concatMapStrings (s: s+"\n") versions;
     in writeTextFile {
       name = "package-versions";
       destination = "/package-versions";
       text = versionText;
     })
  ];
}
