{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.wayland;
in
{
  options.apps.wayland = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable wayland apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        wofi
        wl-clipboard
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
