{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.build-env;

  # Script to rebuild the environment from this file.
  nix-rebuild = pkgs.writeShellScriptBin "nix-rebuild" ''
     set -e

     oldGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
     oldVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

     if ! command -v nix-env &>/dev/null; then
         >&2 echo "warning: nix-env was not found in PATH, add nix to user environment"
         PATH="${pkgs.nix}/bin''${PATH+:$PATH}"
     fi

     if (( $(nix profile list 2>/dev/null | wc -l) > 0 )); then
         nix profile upgrade '.*' --builders "" --recreate-lock-file --print-build-logs "$@"
     else
         nix-env --set -f ~/.config/nixpkgs/default.nix --builders "" "$@"
     fi

     newGeneration=$(readlink "$(readlink ~/.nix-profile)" | cut -d '-' -f 2)
     newVersions=$(readlink ~/.nix-profile/package-versions || echo "/dev/null")

     ${pkgs.diffutils}/bin/diff --color -u \
     --label "generation $oldGeneration" "$oldVersions" \
     --label "generation $newGeneration" "$newVersions" \
     || true
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
       versionText = lib.strings.concatMapStrings (s: s + "\n") versions;
     in
     pkgs.writeTextFile {
       name = "package-versions";
       destination = "/package-versions";
       text = versionText;
     };

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
    paths = cfg.paths ++ [ nix-rebuild package-versions ];
  };
}
