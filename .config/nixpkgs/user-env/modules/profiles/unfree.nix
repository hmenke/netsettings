{ config, lib, pkgs, ... }:

{
  extraPackages = with pkgs; [
    masterpdfeditor-free
    skypeforlinux
    slack
    steam
    steam-run
    zoom-us
  ];
}
