# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  default = {
    withUnfree = false;
    withGui = false;
    withTeXLive = false;
  };

  site = if builtins.pathExists ./buildEnv-config.nix
    then import ./buildEnv-config.nix
    else { };

  config = default // site;
  inherit (config) withUnfree withGui withTeXLive;
in
buildEnv rec {
  name = "user-env";
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths =
    let
      localPackages =
        if builtins.pathExists ./local.nix
        then import ./local.nix pkgs
        else [ ];

      userPackages = localPackages ++ [
        # command line utils
        (aspellWithDicts (dicts: with dicts; [ de en ]))
        bindfs
        cachix
        direnv
        emacs
        file
        fzf
        git-lfs
        gitAndTools.delta
        gitAndTools.gh
        gitAndTools.git-annex
        gitAndTools.git-crypt
        gitAndTools.git-filter-repo
        gitAndTools.gitFull
        gitAndTools.pass-git-helper
        gitAndTools.pre-commit
        glib
        gnuplot_qt
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
        (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
        python3
        python3Packages.black
        qpdf
        rclone
        tree
        ts
        unison
        unzip
        youtube-dl
        zip
      ] ++ lib.lists.optionals withTeXLive [
        texlive-env
      ] ++ lib.lists.optionals withGui [
        # GUI
        browserpass
        (chromium.override {
          commandLineArgs = [
            "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
            "--enable-features=VaapiVideoDecoder"
          ];
        })
        clementine
        element-desktop
        evince
        gnome3.adwaita-icon-theme
        gnome3.evolution
        gnomeExtensions.appindicator
        libnotify
        networkmanagerapplet
        pavucontrol
        pop-os-shell
        slack
        splatmoji
        vlc
        wofi
        wl-clipboard
      ] ++ lib.lists.optionals withUnfree [
        # Proprietary
        masterpdfeditor-free
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

          if ! command -v nix-env &>/dev/null; then
              >&2 echo "warning: nix-env was not found in PATH, add nix to user environment"
              PATH="${nix}/bin''${PATH+:$PATH}"
          fi

          if (( $(nix profile info 2>/dev/null | wc -l) > 0 )); then
              nix profile upgrade '.*' --builders "" --recreate-lock-file --no-write-lock-file --print-build-logs "$@"
          else
              nix-env --set -f ~/.config/nixpkgs/buildEnv.nix --builders "" "$@"
          fi

          newGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
          newVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

          ${diffutils}/bin/diff --color -u \
          --label "generation $oldGeneration" "$oldVersions" \
          --label "generation $newGeneration" "$newVersions" \
          || true
        '')

        # Since you can't see the versions with nix-env -q anymore, we write them
        # to a file for easy querying
        (
          let
            collect = pkgs:
              let
                recurse = x:
                  if lib.isDerivation x then
                    [ x ]
                  else if x.recurseForDerivations or false then
                    collect (lib.attrValues x)
                  else
                    [ ];
              in
              lib.concatMap recurse pkgs;
            versions = map (pkg: pkg.name) (collect userPackages);
            versionText = lib.strings.concatMapStrings (s: s + "\n") versions;
          in
          writeTextFile {
            name = "package-versions";
            destination = "/package-versions";
            text = versionText;
          }
        )
      ];

    in
    userPackages ++ buildEnvHelpers;
}
