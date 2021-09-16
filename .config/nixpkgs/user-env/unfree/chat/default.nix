{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.unfree.chat;
in
{
  options.unfree.chat = {
    enable = mkOption {
      default = config.unfree.enable;
      type = types.bool;
      description = "Whether to enable unfree chat apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        skypeforlinux
        slack
        zoom-us
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
