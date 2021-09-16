{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.unfree.steam;
in
{
  options.unfree.steam = {
    enable = mkOption {
      default = config.unfree.enable;
      type = types.bool;
      description = "Whether to enable Steam";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        steam
        steam-run
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
