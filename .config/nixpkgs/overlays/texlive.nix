self: super: {
  texlive-env = let
    executables = [
      "afm2tfm"
      "authorindex"
      "bibtex"
      "cachepic"
      "dvilualatex"
      "dvilualatex-dev"
      "dviluatex"
      "dvipdfm"
      "dvipdfmx"
      "dvipdft"
      "dvips"
      "ebb"
      "epspdf"
      "epspdftk"
      "etex"
      "exceltex"
      "extractbb"
      "fig4latex"
      "fmtutil"
      "fmtutil-sys"
      "fmtutil-user"
      "getmapdl"
      "gftodvi"
      "gftopk"
      "gftype"
      "inimf"
      "initex"
      "kpseaccess"
      "kpsereadlink"
      "kpsestat"
      "kpsewhich"
      "l3build"
      "latex"
      "latex-dev"
      "luahbtex"
      "lualatex"
      "lualatex-dev"
      "luaotfload-tool"
      "luatex"
      "lwarpmk"
      "makedtx"
      "makeglossaries"
      "makeglossaries-lite"
      "makeindex"
      "mathspic"
      "mf"
      "mf-nowin"
      "mft"
      "mkindex"
      "mkpic"
      "mktexfmt"
      "mktexlsr"
      "mktexmf"
      "mktexpk"
      "mktextfm"
      "mptopdf"
      "pdfannotextractor"
      "pdfatfi"
      "pdfetex"
      "pdflatex"
      "pdflatex-dev"
      "pdftex"
      "perltex"
      "pktogf"
      "pktype"
      "pn2pdf"
      "pygmentex"
      "rungs"
      "simpdftex"
      "splitindex"
      "svn-multi"
      "teckit_compile"
      "tex"
      "texhash"
      "texlua"
      "texluac"
      "thumbpdf"
      "tikztosvg"
      "tlmgr"
      "tlshell"
      "updmap"
      "updmap-sys"
      "updmap-user"
      "vpe"
      "webquiz"
      "wordcount"
      "xdvi"
      "xdvipdfmx"
      "xdvi-xaw"
      "xelatex"
      "xelatex-dev"
      "xetex"
      "yplan"
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
