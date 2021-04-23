self: super: {
  nix-direnv = super.nix-direnv.override { nix = self.nixFlakes; };

  softmaker-office = super.softmaker-office.override {
    officeVersion = {
      version = "976";
      edition = "2018";
      sha256 = "sha256-A45q/irWxKTLszyd7Rv56WeqkwHtWg4zY9YVxqA/KmQ=";
    };
  };

  gitAndTools = super.gitAndTools // {
    pass-git-helper = self.python3Packages.buildPythonApplication rec {
      pname = "pass-git-helper";
      version = "1.1.0";

      src = self.fetchFromGitHub {
        owner = "languitar";
        repo = "pass-git-helper";
        rev = "v${version}";
        sha256 = "18nvwlp0w4aqj268wly60rnjzqw2d8jl0hbs6bkwp3hpzzz5g6yd";
      };

      propagatedBuildInputs = with self.python3Packages; [ pyxdg ];
      checkInputs = with self.python3Packages; [ pytest ];
      preCheck = ''
        export HOME=$(mktemp -d)
      '';
    };
  };

  zoom-us-xcb = self.symlinkJoin {
    name = "zoom-us-xcb";
    paths = [ self.zoom-us ];
    buildInputs = [ self.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/zoom --set QT_QPA_PLATFORM "xcb"
      wrapProgram $out/bin/zoom-us --set QT_QPA_PLATFORM "xcb"
      rm -f "$out/share/applications/Zoom.desktop"
      substitute \
        "${self.zoom-us}/share/applications/Zoom.desktop" \
        "$out/share/applications/Zoom.desktop" \
        --replace "${self.zoom-us}/bin/zoom" "$out/bin/zoom"
    '';
  };
}
