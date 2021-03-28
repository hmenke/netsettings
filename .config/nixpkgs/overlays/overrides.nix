self: super: {
  mathematica-unwrapped = let
    version = "12.1.1";
    name = "Mathematica_${version}_LINUX.sh";
    sha256 = "02mk8gmv8idnakva1nc7r7mx8ld02lk7jgsj1zbn962aps3bhixd";
  in super.mathematica.overrideAttrs (oldAttrs: {
    name = "mathematica-unwrapped-${version}";
    inherit version;
    src = super.requireFile {
      inherit name sha256;
      message = ''
        This nix expression requires that ${name} is
        already part of the store. Find the file on your Mathematica CD
        and add it to the nix store with nix-store --add-fixed sha256 <FILE>.
      '';
    };
  });

  mathematica = let version = "12.1.1";
  in self.stdenv.mkDerivation {
    name = "mathematica-${version}";
    buildCommand =
      let mathematica = "${self.mathematica-unwrapped}/bin/mathematica";
      in ''
        mkdir -p $out/bin
      '' + builtins.foldl' (l: r:
        l + ''
          cat > "$out/bin/${r}" <<EOF
          #! ${self.runtimeShell} -e
          export USE_WOLFRAM_LD_LIBRARY_PATH=1
          export QT_XCB_GL_INTEGRATION=none
          exec /run/wrappers/bin/firejail --noprofile --net=none "${self.mathematica-unwrapped}/bin/${r}" "\$@"
          EOF
          chmod 0755 "$out/bin/${r}"
        '') "" [
          "math"
          "mathematica"
          "Mathematica"
          "MathKernel"
          "mcc"
          "wolfram"
          "WolframKernel"
        ];
  };

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
}
