{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in
{
  options.userPackages = mkOption {
    default = [];
    type = types.listOf types.package;
    description = "List of extra packages to add to the environment.";
  };

  options.extraDependencies = mkOption {
    default = [];
    type = types.listOf types.pathInStore;
    description = "Paths that should be included in the closure but not linked into the profile";
  };

  config.build-env.paths = config.userPackages ++ lib.optional
    (config.extraDependencies != [])
    (pkgs.linkFarm "extra-dependencies"
      (lib.map (drv: {
        name = "extra-dependencies/${drv.name}";
        path = drv;
      }) config.extraDependencies));
}
