{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.shell;
in
{
  options.tools.shell = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable shell utilities";
    };

    package = mkOption {
      default = pkgs.shell;
      type = types.package;
      description = "Shell package";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        fzf
        gnuplot_qt
        tree
        ts
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
