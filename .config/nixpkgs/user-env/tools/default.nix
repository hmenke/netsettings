{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in
{
  imports = [
    ./crypto
    ./editors
    ./formats
    ./git
    ./nix
    ./python
    ./shell
    ./sync
  ];

  options.tools.enable = mkOption {
    default = true;
    type = types.bool;
    description = "Whether to enable all tools";
  };
}

  
