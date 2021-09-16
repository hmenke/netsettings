{
  description = "User environment";

  inputs = {
    modules.url = "git+https://git.henrimenke.de/henri/nixos-modules.git";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, modules, emacs-overlay, ... }: {
    lib.userEnvironment = configuration: import ./default.nix {
      inherit configuration;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      overlays = [
        modules.overlays.system
        modules.overlays.user
        emacs-overlay.overlay
      ];
    };
  };
}
