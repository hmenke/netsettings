{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    discord
    masterpdfeditor4
    skypeforlinux
    slack
  ];
}
