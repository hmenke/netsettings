{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.build-env;

  # Script to rebuild the environment from this file.
  nix-rebuild = pkgs.writeShellScriptBin "nix-rebuild" ''
     set -euo pipefail

     if ! command -v nix-env &>/dev/null; then
         >&2 echo "warning: nix-env was not found in PATH, add nix to user environment"
         PATH="${pkgs.nix}/bin''${PATH+:$PATH}"
     fi

     isFlake=$(( $(nix profile list 2>/dev/null | wc -l) > 0 ))

     instantiate() {
         case "$isFlake" in
             1) nix eval ~/.config/nixpkgs#defaultPackage.x86_64-linux.drvPath "$@" ;;
             0) nix-instantiate ~/.config/nixpkgs/default.nix "$@" ;;
         esac
     }

     build() {
         case "$isFlake" in
             1) nix build --print-build-logs ~/.config/nixpkgs "$@" ;;
             0) nix-build ~/.config/nixpkgs/default.nix "$@" ;;
         esac
     }

     update() {
         case "$isFlake" in
             1) nix flake update --flake ~/.config/nixpkgs "$@" ;;
             0) echo "update has no effect for non-Flakes" ;;
         esac
     }

     switch() {
         oldGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
         oldVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

         case "$isFlake" in
             1) nix profile upgrade --all --print-build-logs "$@" ;;
             0) nix-env --set -f ~/.config/nixpkgs/default.nix "$@" ;;
         esac

         newGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
         newVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

         ${pkgs.diffutils}/bin/diff --color -u \
         --label "generation $oldGeneration" "$oldVersions" \
         --label "generation $newGeneration" "$newVersions" \
         || true
     }

     case "''${1-}" in
         eval) shift; instantiate "$@" ;;
         build) shift; build "$@" ;;
         update) shift; update "$@" ;;
         switch) shift; switch "$@" ;;
         *) case "$isFlake" in
                1) update "$@"; switch "$@" ;;
                0) switch "$@" ;;
            esac ;;
     esac
   '';

   # Since you can't see the versions with nix-env -q anymore, we write them
   # to a file for easy querying
   package-versions =
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
       versions = map (pkg: pkg.name) (collect cfg.paths);
       versionsSorted = lib.sort builtins.lessThan versions;
       versionText = lib.strings.concatMapStrings (s: s + "\n") versionsSorted;
     in
     pkgs.writeTextFile {
       name = "package-versions";
       destination = "/package-versions";
       text = versionText;
     };

   # Add a link to the nixpkgs checkout we built from
   link-nixpkgs = pkgs.linkFarm "nixpkgs" [
     { name = "nixpkgs";
       path = pkgs.path; }
   ];
in
{
  options.build-env = {
    name = mkOption {
      default = "user-env";
      type = types.str;
      description = "Name of the profile derivation";
    };

    extraOutputsToInstall = mkOption {
      default = [ "bin" "lib" "out" ];
      type = types.listOf types.str;
      description = "Extra paths to link into the profile";
    };

    paths = mkOption {
      internal = true;
      default = [];
      type = types.listOf types.package;
      description = "Paths to link into the profile";
    };

    toplevel = mkOption {
      internal = true;
      description = "The top level derivation";
    };
  };

  config.build-env.toplevel = pkgs.buildEnv {
    inherit (cfg) name extraOutputsToInstall;
    paths = cfg.paths ++ [ nix-rebuild package-versions link-nixpkgs ];
  };
}
