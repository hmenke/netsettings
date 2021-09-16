{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.fhs.mathematica;
in
{
  options.fhs.mathematica = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Whether to enable Mathematica FHS";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        mathematica-env
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
