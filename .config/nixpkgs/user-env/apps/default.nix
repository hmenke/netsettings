{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./chat
    ./chromium
    ./gnome
    ./media
    ./office
    ./wayland
  ];

  options.apps.enable = mkOption {
    default = false;
    type = types.bool;
    description = "Whether to enable all apps";
  };
}
