{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.sync;
in
{
  options.tools.sync = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable sync utilities";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        isync
        msmtp
        neomutt
        rclone
        sshuttle
        unison
        youtube-dl
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
