{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    mathematica-env
    texlive-env
  ];
}
