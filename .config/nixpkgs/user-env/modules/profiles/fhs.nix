{ config, lib, pkgs, ... }:

{
  extraPackages = with pkgs; [
    mathematica-env
    texlive-env
  ];
}
