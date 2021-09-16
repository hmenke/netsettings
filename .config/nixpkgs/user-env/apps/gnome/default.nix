{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.gnome;
in
{
  options.apps.gnome = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable gnome apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        gnomeExtensions.appindicator
        gnomeExtensions.gsconnect
        gnomeExtensions.sound-output-device-chooser
        libnotify
        pop-os-shell
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
