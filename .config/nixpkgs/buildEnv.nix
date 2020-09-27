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
  overlays = builtins.map import [
    ./overlays/backports.nix
    ./overlays/newPackages.nix
  ];
  nixexprs = fetchTarball {
    url = "https://nixos.org/channels/nixos-20.03/nixexprs.tar.xz";
  };
  pkgs = import nixexprs { inherit config overlays; };

in

with pkgs;

buildEnv rec {
  inherit name;
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths = let

    localPackages = if builtins.pathExists ./local.nix
                    then import ./local.nix pkgs
                    else [];

    userPackages = localPackages ++ [
      # command line utils
      aspell
      aspellDicts.de
      aspellDicts.en
      cachix
      direnv
      dnsutils
      emacs
      ffmpeg
      file
      fzf
      (hiPrio gcc)
      gdb
      gitAndTools.gitFull
      gnuplotGit
      inetutils
      isync
      jq
      llvmPackages_latest.clang
      msmtp
      neomutt
      neovim
      newsboat
      niv
      nix-direnv
      nixops
      nmap
      openssl
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
      splatmoji
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
      masterpdfeditor-free
      mathematica
      skypeforlinux
      softmaker-office
      zoom-us
    ];

    buildEnvHelpers = [
      # Script to rebuild the environment from this file.
      (writeShellScriptBin "nix-rebuild" ''
        set -e

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

      # Create a link to the nixpkgs that are used to build the environment.
      # Then it is possible to reference this when using nix imperatively without
      # having to redownload the tarball, e.g.
      #
      #     nix run -f ~/.nix-profile/nixpkgs hello -c hello
      #
      (linkFarm "nixpkgs" [ { name = "nixpkgs"; path = nixexprs; } ])

      # Since you can't see the versions with nix-env -q anymore, we write them
      # to a file for easy querying
      (let
         collect = pkgs:
           let recurse = x:
             if lib.isDerivation x then [x]
             else if x.recurseForDerivations or false then collect x
             else [];
           in lib.concatMap recurse pkgs;
         versions = map (pkg: pkg.name) (collect userPackages);
         versionText = lib.strings.concatMapStrings (s: s+"\n") versions;
       in writeTextFile {
         name = "package-versions";
         destination = "/package-versions";
         text = versionText;
       })
    ];

  in userPackages ++ buildEnvHelpers;
}
