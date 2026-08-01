{
  lib,
  stdenv,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication (finalAttrs: {
  pname = "voe-dl";
  version = "1.9.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "p4ul17";
    repo = "voe-dl";
    tag = "v${finalAttrs.version}";
    hash = "sha256-0O9DGCpssaq/FB/+MgTpOmUswNJ4/SFWehwmXremEy0=";
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
