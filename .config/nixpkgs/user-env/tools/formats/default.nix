{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.formats;
in
{
  options.tools.formats = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable file format utilities";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        file
        jq
        pandoc
        qpdf
        unzip
        zip
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
