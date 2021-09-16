{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.chat;
in
{
  options.apps.chat = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable chat apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        element-desktop
        gajim
        signal-desktop
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
