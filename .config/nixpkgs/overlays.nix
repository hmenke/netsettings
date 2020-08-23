[
  #(import (builtins.fetchTarball {
  #  url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #}))

  (self: super: {
    gnuplot_qt = super.gnuplot_qt.overrideAttrs (oldAttrs: {
      version = "5.5+git-ef315e";

      patches = [ (self.writeText "transparency.patch"
        ''
          --- a/term/cairo.trm
          +++ a/term/cairo.trm
          @@ -849,7 +849,7 @@
           	plot.background.g = cairo_params->background.g;
           	plot.background.b = cairo_params->background.b;
           	gp_cairo_set_background(cairo_params->background);
          -	if (ISCAIROLATEX || cairo_params->transparent)
          +	if (cairo_params->transparent)
           		gp_cairo_clear_background(&plot);
           	else
           		gp_cairo_solid_background(&plot);
        '') ];

      src = self.fetchgit {
        url = "https://git.code.sf.net/p/gnuplot/gnuplot-main";
        rev = "ef315e6f148bb3144726246cbf396aad53e3a000";
        sha256 = "143d3hbw8pk2pp3rpplr81ax5cbcrrfhz994zzky5xavdw4z43df";
      };

      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
        self.autoconf
        self.automake
        self.git
      ];

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

    mathematica = let
      version = "12.1.1";
    in self.stdenv.mkDerivation {
      name = "mathematica-${version}";
      buildCommand = let
        mathematica = "${self.mathematica-unwrapped}/bin/mathematica";
      in ''
        mkdir -p $out/bin
      '' + builtins.foldl' (l: r: l + ''
        cat > "$out/bin/${r}" <<EOF
        #! ${self.runtimeShell} -e
        export USE_WOLFRAM_LD_LIBRARY_PATH=1
        export QT_XCB_GL_INTEGRATION=none
        exec /run/wrappers/bin/firejail --noprofile --net=none "${self.mathematica-unwrapped}/bin/${r}" "\$@"
        EOF
        chmod 0755 "$out/bin/${r}" 
      '') "" [ "math" "mathematica" "Mathematica" "MathKernel" "mcc" "wolfram" "WolframKernel" ];
    };
  })
]
