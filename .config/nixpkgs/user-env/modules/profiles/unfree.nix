{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    softmaker-office' = softmaker-office.override {
      officeVersion = {
        version = "976";
        edition = "2018";
        hash = "sha256-A45q/irWxKTLszyd7Rv56WeqkwHtWg4zY9YVxqA/KmQ=";
      };
    };
  in [
    masterpdfeditor4
    softmaker-office'
  ];
}
