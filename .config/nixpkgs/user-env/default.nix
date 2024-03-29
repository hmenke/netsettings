{ configuration
, pkgs ? import <nixpkgs> { }
, overlays ? [ ]
}:

let
  lib = pkgs.lib;

  eval = lib.evalModules {
    modules = [
      {
        config.nixpkgs = {
          pkgs = lib.mkDefault pkgs;
          inherit overlays;
        };
      }
      configuration
      ./modules
    ];
    specialArgs = { modulesPath = builtins.toString ./modules; };
  };

  # From nixpkgs/nixos/modules/system/activation/top-level.nix
  failedAssertions = map (x: x.message) (lib.filter (x: !x.assertion) eval.config.assertions);

  config =
    if failedAssertions != [ ]
    then throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else lib.showWarnings eval.config.warnings eval.config;

in
config.build-env.toplevel
