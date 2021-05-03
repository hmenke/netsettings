# Declarative Package Management
#
# https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
#
# Install using
#
#    nix-env --set -f ~/.config/nixpkgs/buildEnv.nix

{ pkgs ? import <nixpkgs> {
  overlays = let
    modules = builtins.fetchGit "https://git.henrimenke.de/henri/nixos-modules.git";
  in [
    (import "${modules}/overlays/system.nix")
    (import "${modules}/overlays/user.nix")
  ];
} }:
let
  site =
    if builtins.pathExists ./site.nix
    then import ./site.nix
    else { };
in
import ./buildEnv.nix ({ inherit pkgs; } // site)
