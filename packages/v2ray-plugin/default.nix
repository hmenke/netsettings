{ buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "v2ray-plugin-${version}";
  version = "1.3.1";
  goPackagePath = "github.com/shadowsocks/v2ray-plugin";
  src = fetchFromGitHub {
    owner = "shadowsocks";
    repo = "v2ray-plugin";
    rev = "b9717056b251747149cacb44458fe02e420b9d9b";
    sha256 = "0aq445gnqk9dxs1hkw7rvk86wg0iyiy0h740lvyh6d9zsqhf61wb";
  };
  goDeps = ./deps.nix;
}
