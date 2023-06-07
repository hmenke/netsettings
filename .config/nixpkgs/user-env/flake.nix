{
  description = "User environment";

  inputs = {
    modules = {
      url = "git+https://git.henrimenke.de/henri/nixos-modules.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, modules, ... }: {
    lib.userEnvironment = configuration: import ./default.nix {
      inherit configuration;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      overlays = [
        modules.overlays.system
        modules.overlays.user
      ];
    };
  };
}
