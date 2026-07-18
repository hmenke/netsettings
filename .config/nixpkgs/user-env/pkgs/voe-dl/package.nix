{
  lib,
  stdenv,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication (finalAttrs: {
  pname = "voe-dl";
  version = "1.8.2";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "p4ul17";
    repo = "voe-dl";
    tag = "v${finalAttrs.version}";
    hash = "sha256-agX3TjEQ9uDy6z3ZMHU73uyCRhRlhQKR0y1sDwZlB0M=";
  };

  build-system = with python3Packages; [ hatchling ];

  dependencies = with python3Packages; [
    beautifulsoup4
    requests
    wget
    yt-dlp
  ];

  meta = {
    description = "A Python downloader for voe.sx videos";
    mainProgram = "voe-dl";
    homepage = "https://github.com/p4ul17/voe-dl";
    changelog = "https://github.com/p4ul17/voe-dl/blob/${finalAttrs.src.tag}/CHANGELOG.md";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [
      hmenke
    ];
  };
})
