{
  config,
  lib,
  pkgs,
  ...
}:

{
  userPackages = with pkgs; [
    llama-cpp
    pi-coding-agent
  ];
}
