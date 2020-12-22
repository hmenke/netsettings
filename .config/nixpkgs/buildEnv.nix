# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

{ name ? "user-env", withUnfree ? true, withGui ? true }:

let

  pkgs = import <nixpkgs> { };

in with pkgs;

buildEnv rec {
  inherit name;
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths = let

    localPackages =
      if builtins.pathExists ./local.nix then import ./local.nix pkgs else [ ];

    userPackages = localPackages ++ [
      # command line utils
      aspell
      aspellDicts.de
      aspellDicts.en
      cachix
      direnv
      emacs
      file
      fzf
      git-lfs
      gitAndTools.delta
      gitAndTools.diff-highlight
      gitAndTools.gh
      gitAndTools.git-crypt
      gitAndTools.git-filter-repo
      gitAndTools.gitFull
      gitAndTools.pass-git-helper
      glib
      gnuplotGit
      gvfs
      isync
      jq
      msmtp
      neomutt
      neovim
      niv
      nix-direnv
      nix-index
      nixpkgs-fmt
      openssl
      pandoc
      pass-otp
      python3
      python3Packages.black
      qpdf
      tree
      ts
      unison
      unzip
      youtube-dl
      zip
    ] ++ lib.lists.optionals (builtins.pathExists /opt/texlive) [
      texlive-env
    ] ++ lib.lists.optionals withGui [
      # GUI
      arandr
      clementine
      dunst
      evince
      feh
      firefox
      gnome3.adwaita-icon-theme
      gnome3.evolution
      libnotify
      networkmanagerapplet
      pavucontrol
      polybarFull
      rofi
      scrot
      slack
      splatmoji
      vlc
      xclip
      xorg.setxkbmap
      xorg.xbacklight
      xorg.xmodmap
      xorg.xsetroot
      xscreensaver
    ] ++ lib.lists.optionals withUnfree [
      # Proprietary
      dropbox
      google-chrome
      masterpdfeditor-free
      mathematica
      skypeforlinux
      softmaker-office
      steam
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
        --arg withUnfree ${lib.trivial.boolToString withUnfree} \
        --arg withGui ${lib.trivial.boolToString withGui} \
        --builders "" \
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
          throw '''
            Your user environment is a buildEnv which is incompatible with
            nix-env's built-in env builder. Edit your home expression and run
            nix-rebuild instead!
          '''
        '';
      })

      # Since you can't see the versions with nix-env -q anymore, we write them
      # to a file for easy querying
      (let
        collect = pkgs:
          let
            recurse = x:
              if lib.isDerivation x then
                [ x ]
              else if x.recurseForDerivations or false then
                collect (lib.attrValues x)
              else
                [ ];
          in lib.concatMap recurse pkgs;
        versions = map (pkg: pkg.name) (collect userPackages);
        versionText = lib.strings.concatMapStrings (s: s + "\n") versions;
      in writeTextFile {
        name = "package-versions";
        destination = "/package-versions";
        text = versionText;
      })
    ];

  in userPackages ++ buildEnvHelpers;
}
