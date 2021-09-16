{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.office;
in
{
  options.apps.office = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable office apps";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        evince
        evolution
        gimp
        onlyoffice-bin
        xournalpp
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
