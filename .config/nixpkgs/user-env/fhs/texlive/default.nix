{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.fhs.texlive;
in
{
  options.fhs.texlive = {
    enable = mkOption {
      default = config.fhs.enable;
      type = types.bool;
      description = "Whether to enable TeX Live FHS";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        texlive-env
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
