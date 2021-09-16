{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.python;
in
{
  options.tools.python = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable Python";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        python3
        python3Packages.black
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
