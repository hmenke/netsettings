{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./mathematica
    ./texlive
  ];

  options.fhs.enable = mkOption {
    default = false;
    type = types.bool;
    description = "Whether to enable all FHS environments";
  };
}
