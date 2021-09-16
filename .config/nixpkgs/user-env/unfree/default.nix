{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./chat
    ./office
    ./steam
  ];

  options.unfree.enable = mkOption {
    default = false;
    type = types.bool;
    description = "Whether to enable all unfree apps";
  };
}
