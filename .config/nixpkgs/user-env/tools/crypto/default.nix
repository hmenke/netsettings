{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.tools.crypto;
in
{
  options.tools.crypto = {
    enable = mkOption {
      default = config.tools.enable;
      type = types.bool;
      description = "Whether to enable encryption utilities";
    };

    package = mkOption {
      default = pkgs.crypto;
      type = types.package;
      description = "Crypto package";
    };

    packages = mkOption {
      internal = true;
      default = with pkgs; [
        openssl
        pass-git-helper
        (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
