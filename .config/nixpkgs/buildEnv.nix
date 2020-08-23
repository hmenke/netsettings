# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

let

  overlays = import ./overlays.nix;
  importChannel = channel: import (fetchTarball { url = "https://nixos.org/channels/${channel}/nixexprs.tar.xz"; });
  pkgs = importChannel "nixos-20.03" { inherit overlays; };
  unstable = importChannel "nixpkgs-unstable" { inherit overlays; };

in

{ name ? "user-env" }:

with pkgs;

buildEnv {
  inherit name;
  extraOutputsToInstall = [ "bin" "lib" "out" ];
  paths = [
    nix # If not on NixOS, this is important!
  ] ++ [
    cachix
    direnv
    gnuplot_qt
    niv
    nix-direnv
    nixops
    unstable.mathematica
    vgo2nix
  ] ++ [
    # Script to rebuild the environment from this file.
    (writeScriptBin "nix-rebuild" ''
      #!${runtimeShell}
      nix-env --set -f ~/.config/nixpkgs/buildEnv.nix --argstr name "$(whoami)-user-env-$(date -I)"
    '')

    # Manifest to make sure imperative nix-env doesn't work (otherwise it will
    # overwrite the profile, removing all packages other than the
    # newly-installed one).
    (writeTextFile {
      name = "break-nix-env-manifest";
      destination = "/manifest.nix";
      text = ''
        throw ''\''
          Your user environment is a buildEnv which is incompatible with
          nix-env's built-in env builder. Edit your home expression and run
          nix-rebuild instead!
        ''\''
      '';
    })
    # To allow easily seeing which nixpkgs version the profile was built from,
    # place the version string in ~/.nix-profile/nixpkgs-version
    (writeTextFile {
      name = "nixpkgs-version";
      destination = "/nixpkgs-version";
      text = lib.version;
    })
  ];
}
