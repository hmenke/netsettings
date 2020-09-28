self: super: {
  texlive-env = let
    executables = [
      "afm2tfm"
      "bibtex"
      "dvilualatex"
      "dviluatex"
      "dvipdfm"
      "dvipdfmx"
      "dvipdft"
      "dvips"
      "ebb"
      "etex"
      "extractbb"
      "fmtutil"
      "fmtutil-sys"
      "fmtutil-user"
      "gftodvi"
      "gftopk"
      "gftype"
      "inimf"
      "initex"
      "kpseaccess"
      "kpsereadlink"
      "kpsestat"
      "kpsewhich"
      "latex"
      "luahbtex"
      "lualatex"
      "luaotfload-tool"
      "luatex"
      "makeindex"
      "man"
      "mf"
      "mf-nowin"
      "mft"
      "mkindex"
      "mktexfmt"
      "mktexlsr"
      "mktexmf"
      "mktexpk"
      "mktextfm"
      "mptopdf"
      "pdfetex"
      "pdflatex"
      "pdftex"
      "pktogf"
      "pktype"
      "rungs"
      "simpdftex"
      "tex"
      "texhash"
      "texlua"
      "texluac"
      "tlmgr"
      "tlshell"
      "updmap"
      "updmap-sys"
      "updmap-user"
      "xdvi"
      "xdvipdfmx"
      "xdvi-xaw"
    ];

    fhs = self.buildFHSUserEnv {
      name = "texlive-fhs";
      targetPkgs = pkgs: with pkgs; [
        fontconfig
        freetype
        perl
        tk
        wget
      ];
      runScript = "";
      profile = ''
        export PATH="/opt/texlive/current/bin/x86_64-linux''${PATH:+:$PATH}"
      '';
    };

    makeFhsWrapper = name: self.writeShellScriptBin name ''
      exec "${fhs}/bin/texlive-fhs" "${name}" "$@"
    '';

  in self.buildEnv {
    name = "texlive-env";
    paths = [ fhs ] ++ (map makeFhsWrapper executables);
  };
}
