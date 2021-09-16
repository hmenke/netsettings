{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.nix;
in
{
  options.tools.nix = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable nix";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        cachix
        direnv
        nix-direnv
        nix-index
        nixpkgs-fmt
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
