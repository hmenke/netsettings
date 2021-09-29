{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    masterpdfeditor-free
    skypeforlinux
    slack
    steam
    steam-run
    zoom-us
  ];
}
