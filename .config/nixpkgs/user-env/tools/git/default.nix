{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.git;
in
{
  options.tools.git = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable git";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        delta
        gh
        git-annex
        git-crypt
        git-filter-repo
        git-lfs
        gitFull
        lab
        pre-commit
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
