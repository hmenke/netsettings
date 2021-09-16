{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.media;
in
{
  options.apps.media = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable media apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        clementine
        vlc
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
