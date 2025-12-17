{ config, options, lib, ... }:

with lib;

let
  inherit (lib) mkOption types;
  cfg = config.nixpkgs;
  opt = options.nixpkgs;
in
{
  options.nixpkgs = {
    pkgs = mkOption {
      type = types.unspecified;
      description = "pkgs argument to all modules";
    };

    config = mkOption {
      default = {};
      type = types.attrsOf types.unspecified;
      description = "The configuration of the Nix Packages collection.";
    };

    overlays = mkOption {
      default = [];
      type = types.listOf types.unspecified;
      description = "List of overlays to use with the Nix Packages collection.";
    };

    system = mkOption {
      default = cfg.pkgs.stdenv.hostPlatform.system;
      type = types.str;
      description = "Specifies the platform on which to build.";
    };
  };

  config = {
    _module.args = let
      finalPkgs = import cfg.pkgs.path {
        inherit (cfg) config overlays system;
      };
    in {
      inherit (finalPkgs) lib pkgs;
    };
  };
}
