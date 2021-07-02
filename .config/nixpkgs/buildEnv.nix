{ pkgs ? import <nixpkgs> { }
, withUnfree ? false
, withGui ? false
, withFHSEnv ? false
, extraPackages ? (pkgs: [])
}:
  
with pkgs;

buildEnv rec {
  name = "user-env";
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths =
    let
      localPackages = extraPackages pkgs;

      userPackages = localPackages ++ [
        # command line utils
        (aspellWithDicts (dicts: with dicts; [ de en ]))
        cachix
        delta
        direnv
        emacs
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
        sshuttle
        tree
        ts
        unison
        unzip
        youtube-dl
        zip
      ] ++ lib.lists.optionals withFHSEnv [
        texlive-env
        mathematica-env
      ] ++ lib.lists.optionals withGui [
        # GUI
        browserpass
        (chromium.override {
          commandLineArgs = [
            "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
            "--disable-gpu-shader-disk-cache"
            "--enable-features=VaapiVideoDecoder,Vulkan"
            "--enable-gpu-rasterization"
            "--enable-zero-copy"
            "--use-vulkan"
          ];
        })
        clementine
        element-desktop
        evince
        evolution
        gimp
        gnome.adwaita-icon-theme
        gnomeExtensions.appindicator
        gnomeExtensions.gsconnect
        libnotify
        networkmanagerapplet
        pavucontrol
        pop-os-shell
        signal-desktop
        slack
        splatmoji
        virt-manager
        vlc
        wofi
        wl-clipboard
        xournalpp
      ] ++ lib.lists.optionals withUnfree [
        # Proprietary
        masterpdfeditor-free
        skypeforlinux
        softmaker-office
        steam
        steam-run
        zoom-us-xcb
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

          if (( $(nix profile list 2>/dev/null | wc -l) > 0 )); then
              nix profile upgrade '.*' --builders "" --recreate-lock-file --print-build-logs "$@"
          else
              nix-env --set -f ~/.config/nixpkgs/default.nix --builders "" "$@"
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
