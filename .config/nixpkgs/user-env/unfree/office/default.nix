{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.unfree.office;
in
{
  options.unfree.office = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable unfree office apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        masterpdfeditor-free
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
