{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    discord
    masterpdfeditor-free
    skypeforlinux
    slack
  ];
}
