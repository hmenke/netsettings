self: super: {
  gnuplotGit = (super.gnuplot.override {
    withQt = true;
    withLua = true;
  }).overrideAttrs (oldAttrs: {
    version = "5.5+git-fdbde3";

    src = self.fetchgit {
      url = "https://git.code.sf.net/p/gnuplot/gnuplot-main";
      rev = "fdbde39fad8f8309468c0351f975a14ae88f5ddb";
      sha256 = "1fg4h2xzakjrv8df76y2bg5a11k9y8z9plsnpkr4k07jn5iwm5db";
    };

    nativeBuildInputs = oldAttrs.nativeBuildInputs
      ++ [ self.autoconf self.automake ];

    postPatch = ''
      ./prepare
      ${oldAttrs.postPatch}
    '';
  });

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

  softmaker-office = let
    version = "976";
    edition = "2018";
  in self.callPackage
  "${self.path}/pkgs/applications/office/softmaker/generic.nix" {
    pname = "softmaker-office";
    inherit version edition;
    suiteName = "SoftMaker Office";
    src = self.fetchurl {
      url =
        "https://www.softmaker.net/down/softmaker-office-${edition}-${version}-amd64.tgz";
      sha256 = "sha256:14qnlbczq1zcz24vwy2yprdvhyn6bxv1nc1w6vjyq8w5jlwqsgbr";
    };
    archive = "office${edition}.tar.lzma";
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
