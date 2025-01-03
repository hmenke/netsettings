{ config, lib, pkgs, ... }:

with pkgs;

let
  mathematica-env =
    let
      executables = [ "math" "mathematica" "Mathematica" "MathKernel" "mcc"
        "wolfram" "WolframKernel" "wolframscript" ];

      fhs =
        buildFHSEnv {
          name = "mathematica-fhs";
          targetPkgs = pkgs: with pkgs; [
            # only needed during installtion
            bashInteractive
            coreutils
          ] ++ mathematica.buildInputs ++ [
            dbus
            gcc-unwrapped.lib
            zlib
          ];
          unshareNet = true;
          extraOutputsToInstall = [ "out" "lib" ];
          runScript = "";
          profile = ''
            export PATH="/opt/Wolfram/Mathematica/14.0/Executables''${PATH:+:$PATH}"
            export USE_WOLFRAM_LD_LIBRARY_PATH=1
            export QT_QPA_PLATFORM="wayland;xcb"
          '';
        };

      makeFhsWrapper = name:
        writeShellScriptBin name ''
          exec "${fhs}/bin/mathematica-fhs" "${name}" "$@"
        '';

    in
    buildEnv {
      name = "mathematica-env";
      paths = [ fhs ] ++ (map makeFhsWrapper executables);
    };

  texlive-env =
    let
      executables = [ "a2ping" "a5toa4" "adhocfilelist" "afm2afm" "afm2pl"
        "afm2tfm" "aleph" "allcm" "allec" "allneeded" "amstex" "arara" "arlatex"
        "asy" "authorindex" "autoinst" "autosp" "axohelp" "bbl2bib" "bbox"
        "bg5conv" "bg5+latex" "bg5latex" "bg5+pdflatex" "bg5pdflatex" "bib2gls"
        "bibdoiadd" "biber" "bibexport" "bibmradd" "bibtex" "bibtex8" "bibtexu"
        "biburl2doi" "bibzbladd" "bundledoc" "cachepic" "cef5conv" "cef5latex"
        "cef5pdflatex" "cefconv" "ceflatex" "cefpdflatex" "cefsconv" "cefslatex"
        "cefspdflatex" "cfftot1" "checkcites" "checklistings" "chkdvifont"
        "chklref" "chktex" "chkweb" "cjk-gs-integrate" "cllualatex" "cluttex"
        "clxelatex" "context" "contextjit" "convbkmk" "convertgls2bib" "cslatex"
        "csplain" "ctanbib" "ctangle" "ctanify" "ctan-o-mat" "ctanupload" "ctie"
        "ctwill" "ctwill-refsort" "ctwill-twinx" "cweave" "de-macro"
        "depythontex" "detex" "devnag" "deweb" "diadia" "disdvi" "dosepsbin"
        "dt2dv" "dtxgen" "dv2dt" "dvi2fax" "dvi2tty" "dviasm" "dvibook"
        "dviconcat" "dvicopy" "dvidvi" "dvigif" "dvihp" "dviinfox" "dvilj"
        "dvilj2p" "dvilj4" "dvilj4l" "dvilj6" "dvilualatex" "dvilualatex-dev"
        "dviluatex" "dvipdfm" "dvipdfmx" "dvipdft" "dvipng" "dvipos" "dvips"
        "dvired" "dviselect" "dvispc" "dvisvgm" "dvitodvi" "dvitomp" "dvitype"
        "e2pall" "ebb" "ebong" "eplain" "epsffit" "epspdf" "epspdftk" "epstopdf"
        "eptex" "etex" "euptex" "exceltex" "extconv" "extractbb" "extractres"
        "fig4latex" "findhyph" "fmtutil" "fmtutil-sys" "fmtutil-user" "fontinst"
        "fragmaster" "gbklatex" "gbkpdflatex" "getmapdl" "gftodvi" "gftopk"
        "gftype" "git-latexdiff" "gregorio" "gsftopk" "hbf2gf" "ht" "htcontext"
        "htlatex" "htmex" "httex" "httexi" "htxelatex" "htxetex"
        "hyperxmp-add-bytecount" "includeres" "inimSehr geehrte Frau Kaiser,f" "initex" "installfont-tl"
        "jadetex" "jamo-normalize" "jfmutil" "kanji-config-updmap"
        "kanji-config-updmap-sys" "kanji-config-updmap-user"
        "kanji-fontmap-creator" "ketcindy" "komkindex" "kpseaccess" "kpsepath"
        "kpsereadlink" "kpsestat" "kpsetool" "kpsewhere" "kpsewhich" "kpsexpand"
        "l3build" "lacheck" "latex" "latex2man" "latex2nemeth" "latexdef"
        "latex-dev" "latexdiff" "latexdiff-vc" "latexfileversion"
        "latex-git-log" "latexindent" "latexmk" "latexpand" "latex-papersize"
        "latexrevise" "lily-glyph-commands" "lily-image-commands"
        "lily-rebuild-pdfs" "listbib" "listings-ext.sh" "llmk" "lollipop"
        "ltx2crossrefxml" "ltxfileinfo" "ltximg" "luacsplain" "luahbtex"
        "luajithbtex" "luajittex" "lualatex" "lualatex-dev" "luaotfload-tool"
        "luatex" "luatools" "lwarpmk" "mag" "make4ht" "makedtx" "makeglossaries"
        "makeglossaries-lite" "makeindex" "makejvf" "match_parens" "mathspic"
        "mendex" "mex" "mf" "mf2pt1" "mflua" "mfluajit" "mfluajit-nowin"
        "mflua-nowin" "mf-nowin" "mfplain" "mft" "mk4ht" "mkgrkindex" "mkindex"
        "mkjobtexmf" "mkocp" "mkofm" "mkpic" "mkt1font" "mktexfmt" "mktexlsr"
        "mktexmf" "mktexpk" "mktextfm" "mllatex" "mltex" "mmafm" "mmpfb" "mpost"
        "mptopdf" "msxlint" "m-tx" "mtxrun" "mtxrunjit" "multibibliography"
        "musixflx" "musixtex" "odvicopy" "odvitype" "ofm2opl" "omfonts"
        "opl2ofm" "optex" "ot2kpx" "otangle" "otfinfo" "otftotfm" "otp2ocp"
        "outocp" "ovf2ovp" "ovp2ovf" "pamphletangler" "patgen" "pbibtex"
        "pdfannotextractor" "pdfatfi" "pdfbook2" "pdfclose" "pdfcrop"
        "pdfcslatex" "pdfcsplain" "pdfetex" "pdfjadetex" "pdfjam" "pdflatex"
        "pdflatex-dev" "pdflatexpicscale" "pdfmex" "pdfopen" "pdftex"
        "pdftex-quiet" "pdftosrc" "pdfxmltex" "pdfxup" "pdvitomp" "pdvitype"
        "pedigree" "perltex" "pfarrei" "pfb2pfa" "pk2bm" "pkfix" "pkfix-helper"
        "pktogf" "pktype" "platex" "platex-dev" "pltotf" "pmpost" "pmxab"
        "pmxchords" "pn2pdf" "pooltype" "ppltotf" "prepmx" "ps2eps" "ps2frag"
        "ps2pk" "ps4pdf" "psbook" "psjoin" "pslatex" "psnup" "psresize"
        "psselect" "pst2pdf" "pstops" "ptex" "ptex2pdf" "ptftopl" "purifyeps"
        "pygmentex" "pythontex" "repstopdf" "r-mpost" "rpdfcrop" "r-pmpost"
        "rubibtex" "rubikrotation" "rumakeindex" "rungs" "r-upmpost" "scor2prt"
        "simpdftex" "sjisconv" "sjislatex" "sjispdflatex" "spix" "splitindex"
        "srcredact" "sty2dtx" "svn-multi" "synctex" "t1ascii" "t1asm" "t1binary"
        "t1disasm" "t1dotlessj" "t1lint" "t1mac" "t1rawafm" "t1reencode"
        "t1testpage" "t1unmac" "t4ht" "tangle" "teckit_compile" "tex" "tex2aspc"
        "tex2xindy" "tex4ebook" "tex4ht" "texconfig" "texconfig-dialog"
        "texconfig-sys" "texcount" "texdef" "texdiff" "texdirflatten" "texdoc"
        "texdoctk" "texexec" "texfot" "texhash" "texindy" "texlinks"
        "texliveonfly" "texloganalyser" "texlua" "texluac" "texluajit"
        "texluajitc" "texmfstart" "texosquery" "texosquery-jre5"
        "texosquery-jre8" "texplate" "texsis" "tftopl" "thumbpdf" "tie"
        "tikztosvg" "tlcockpit" "tlmgr" "tlshell" "tpic2pdftex" "ttf2afm"
        "ttf2kotexfont" "ttf2pk" "ttf2tfm" "ttfdump" "ttftotype42"
        "typeoutfileinfo" "ulqda" "upbibtex" "updmap" "updmap-sys" "updmap-user"
        "updvitomp" "updvitype" "uplatex" "uplatex-dev" "upmendex" "upmpost"
        "uppltotf" "uptex" "uptftopl" "urlbst" "utf8mex" "vftovp" "vlna" "vpe"
        "vpl2ovp" "vpl2vpl" "vptovf" "weave" "webquiz" "wofm2opl" "wopl2ofm"
        "wordcount" "wovf2ovp" "wovp2ovf" "xasy" "xdvi" "xdvipdfmx" "xdvi-xaw"
        "xelatex" "xelatex-dev" "xetex" "xhlatex" "xindex" "xindy" "xindy.mem"
        "xindy.run" "xmltex" "yplan" ];

      fhs = buildFHSEnv {
        name = "texlive-fhs";
        targetPkgs = pkgs: with pkgs; [ fontconfig freetype libxcrypt-legacy perl tk wget ];
        runScript = "";
        dieWithParent = false;
        profile = ''
          export PATH="/opt/texlive/current/bin/x86_64-linux''${PATH:+:$PATH}"
        '';
      };

      makeFhsWrapper = name:
        writeShellScriptBin name ''
          exec "${fhs}/bin/texlive-fhs" "${name}" "$@"
        '';

    in
    buildEnv {
      name = "texlive-env";
      paths = [ fhs ] ++ (map makeFhsWrapper executables);
    };
in
{
  userPackages = with pkgs; [
    mathematica-env
    texlive-env
  ];
}
