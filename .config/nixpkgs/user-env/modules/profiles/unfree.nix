{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    (discord.overrideAttrs (_: lib.optionalAttrs (lib.versionOlder discord.version "0.0.18") rec {
      pname = "discord";
      version = "0.0.18";
      name = "${pname}-${version}";
      src = fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "1hl01rf3l6kblx5v7rwnwms30iz8zw6dwlkjsx2f1iipljgkh5q4";
      };
    }))
    masterpdfeditor4
    slack
    zoom-us
  ];
}
