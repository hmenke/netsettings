# Declarative Package Management
#
# Install using
#
#    nix profile install . --show-trace --no-write-lock-file --print-build-logs
{
  description = "User flake";

  inputs.modules.url = "git+https://git.henrimenke.de/henri/nixos-modules.git";

  outputs = { self, nixpkgs, modules, ... }:
    let
      system = "x86_64-linux";

      config =
        if builtins.pathExists ./config.nix
        then import ./config.nix
        else { };

      overlays = [ modules.overlays.user ];

      pkgs = import nixpkgs {
        inherit config overlays system;
      };

      site =
        if builtins.pathExists ./site.nix
        then import ./site.nix
        else { };
    in
    {
      packages.${system}.user-env = import ./buildEnv.nix ({ inherit pkgs; } // site);
      defaultPackage.${system} = self.packages.${system}.user-env;
    };
}
