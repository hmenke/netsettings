{
  description = "User environment";

  inputs = {
    modules = {
      url = "git+https://git.henrimenke.de/henri/nixos-modules.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-branchless = {
      url = "github:arxanas/git-branchless";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, modules, emacs-overlay, git-branchless, ... }: {
    lib.userEnvironment = configuration: import ./default.nix {
      inherit configuration;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      overlays = [
        modules.overlays.system
        modules.overlays.user
        emacs-overlay.overlay
        git-branchless.overlay
      ];
    };
  };
}
