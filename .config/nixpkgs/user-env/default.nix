{ configuration
, pkgs ? import <nixpkgs> { }
, overlays ? (
    let
      modules = builtins.fetchTarball "https://git.henrimenke.de/henri/nixos-modules/archive/master.tar.gz";
      emacs-overlay = builtins.fetchTarball "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    in
    [
      (import "${modules}/overlays/system.nix")
      (import "${modules}/overlays/user.nix")
      (import "${emacs-overlay}/default.nix")
    ]
  )
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
