{ config, options, lib, ... }:

with lib;

let
  inherit (lib) mkOption types;
in
{
  options.userPackages = mkOption {
    default = [];
    type = types.listOf types.package;
    description = "List of extra packages to add to the environment.";
  };

  config.build-env.paths = config.userPackages;
}
