# Declarative Package Management
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/default.nix

import ./user-env {
  configuration = {
    _module.args.inputs = null;
    imports = [ ./configuration.nix ];
    nixpkgs.config = import ./config.nix;
  };
}
