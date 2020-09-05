self: super: {
  masterpdfeditor-free = self.stdenv.mkDerivation rec {
    pname = "masterpdfeditor-free";
    version = "4.3.89";

    src = builtins.fetchurl {
      url = "https://code-industry.net/public/master-pdf-editor-${version}_qt5.amd64.tar.gz";
      sha256 = "0k5bzlhqglskiiq86nmy18mnh5bf2w3mr9cq3pibrwn5pisxnxxc";
    };

    nativeBuildInputs = with self; [ autoPatchelfHook qt5.wrapQtAppsHook ];

    buildInputs = with self; [ nss qt5.qtbase qt5.qtsvg sane-backends stdenv.cc.cc ];

    dontStrip = true;

    installPhase = ''
      runHook preInstall

      p=$out/opt/masterpdfeditor
      mkdir -p $out/bin

      substituteInPlace masterpdfeditor4.desktop \
        --replace 'Exec=/opt/master-pdf-editor-4' "Exec=$out/bin" \
        --replace 'Path=/opt/master-pdf-editor-4' "Path=$out/bin" \
        --replace 'Icon=/opt/master-pdf-editor-4' "Icon=$out/share/pixmaps"

      install -Dm644 -t $out/share/pixmaps      masterpdfeditor4.png
      install -Dm644 -t $out/share/applications masterpdfeditor4.desktop
      install -Dm755 -t $p                      masterpdfeditor4
      install -Dm644 license.txt $out/share/$name/LICENSE
      ln -s $p/masterpdfeditor4 $out/bin/masterpdfeditor4
      cp -v -r stamps templates lang fonts $p

      runHook postInstall
    '';

    meta = with self.stdenv.lib; {
      description = "Master PDF Editor";
      homepage = "https://code-industry.net/free-pdf-editor/";
      license = licenses.unfreeRedistributable;
      platforms = with platforms; [ "x86_64-linux" ];
      maintainers = with maintainers; [ hmenke ];
    };
  };

  splatmoji = self.stdenv.mkDerivation rec {
    pname = "splatmoji";
    version = "1.1.0";

    src = self.fetchFromGitHub {
      owner = "cspeterson";
      repo = "splatmoji";
      rev = "v${version}";
      sha256 = "0fdnz290a0dp7adrl64pqcppr419x9xibh6pspcjyxwxp7h66a3x";
    };

    buildInputs = with self; [
      bashInteractive
      rofi
      xdotool
      xsel
    ];

    installPhase = ''
      mkdir -p $out/bin $out/share/splatmoji
      cp -r * $out/share/splatmoji
      cat <<EOF > $out/share/splatmoji/splatmoji.config
      rofi_command=${self.rofi}/bin/rofi -dmenu -p "" -i -theme gruvbox-dark
      xsel_command=${self.xsel}/bin/xsel -b -i
      xdotool_command=${self.xdotool}/bin/xdotool sleep 0.2 type --delay 100
      EOF
      ln -s $out/share/splatmoji/splatmoji $out/bin/splatmoji
    '';
  };
}
