# Declarative Package Management
#
# Install using
#
#    nix profile install . --show-trace --no-write-lock-file --print-build-logs
{
  description = "User flake";

  outputs = { self, nixpkgs, ... }:
    with builtins; 
    let
      system = "x86_64-linux";

      config =
        if pathExists ./config.nix
        then import ./config.nix
        else { };

      overlays = let
        files = attrNames (readDir ./overlays);
        files' = filter (name: null != match ".*\.nix$" name) files;
        overlays = map (name: import (./overlays + "/${name}")) files';
      in overlays;

      pkgs = import nixpkgs {
        inherit config overlays system;
      };
    in
    {
      packages.${system}.user-env = import ./buildEnv.nix { inherit pkgs; };
      defaultPackage.${system} = self.packages.${system}.user-env;
    };
}
