# Declarative Package Management
#
# Install using
#
#    nix profile install . --show-trace --no-write-lock-file --print-build-logs
{
  description = "User flake";

  inputs.user-env = {
    url = "path:./user-env";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, user-env, ... }@inputs: {
    defaultPackage.x86_64-linux = user-env.lib.userEnvironment {
      _module.args.inputs = inputs;
      imports = [ ./configuration.nix ];
      nixpkgs.config = import ./config.nix;
    };
  };
}
