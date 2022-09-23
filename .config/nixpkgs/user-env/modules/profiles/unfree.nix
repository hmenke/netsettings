{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    (discord.overrideAttrs (_: lib.optionalAttrs (lib.versionOlder discord.version "0.0.20") rec {
      pname = "discord";
      version = "0.0.20";
      name = "${pname}-${version}";
      src = fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "3f7yuxigEF3e8qhCetCHKBtV4XUHsx/iYiaCCXjspYw=";
      };
    }))
    masterpdfeditor4
    slack
    (softmaker-office.override {
      officeVersion = {
        version = "976";
        edition = "2018";
        hash = "sha256-A45q/irWxKTLszyd7Rv56WeqkwHtWg4zY9YVxqA/KmQ=";
      };
    })
    zoom-us
  ];
}
