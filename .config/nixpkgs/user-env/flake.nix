{
  description = "User environment";

  outputs = { self, nixpkgs, ... }: {
    lib.userEnvironment = configuration: import ./default.nix {
      inherit configuration;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    };
  };
}
