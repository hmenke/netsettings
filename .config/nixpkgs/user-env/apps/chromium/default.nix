{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption types;
  cfg = config.apps.chromium;
in
{
  options.apps.chromium = {
    enable = mkOption {
      default = config.apps.enable;
      type = types.bool;
      description = "Whether to enable Chromium";
    };

    commandLineArgs = mkOption {
      default = [
        "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
        "--disable-gpu-shader-disk-cache"
        "--enable-features=WebRTCPipeWireCapturer,VaapiVideoDecoder,Vulkan"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
        "--use-vulkan"
      ];
      type = types.listOf types.str;
      description = "Command line flags for the Chromium browser";
    };
        
    packages = mkOption {
      internal = true;
      default = with pkgs; [
        browserpass
        (chromium.override { inherit (cfg) commandLineArgs; })
      ];
      type = types.listOf types.package;
      description = "List of packages";
    };
  };

  config = mkIf cfg.enable {
    build-env.paths = cfg.packages;
  };
}
