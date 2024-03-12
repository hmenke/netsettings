{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    chromium' = chromium.override {
      commandLineArgs = [
        "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
        "--disable-gpu-shader-disk-cache"
        "--enable-features=WebRTCPipeWireCapturer,VaapiVideoDecoder,Vulkan"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
        "--use-vulkan"
      ];
    };
  in [
    browserpass
    chromium'
    evince
    evolution
    gimp
    gnome-frog
    gnomeExtensions.appindicator
    gnomeExtensions.bing-wallpaper-changer
    gnomeExtensions.gsconnect
    gnomeExtensions.pop-shell
    inkscape
    libnotify
    mousai
    songrec
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xdragon
    xournalpp
  ];
}
