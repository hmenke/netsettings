{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.editors;
in
{
  options.tools.editors = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable editors";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        (aspellWithDicts (dicts: with dicts; [ de en ]))
        emacsPgtkGcc
        glib
        gvfs
        neovim
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
