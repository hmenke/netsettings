{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    chromium' = chromium.override {
      commandLineArgs = [
        "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
        "--disable-gpu-shader-disk-cache"
        "--enable-features=WebRTCPipeWireCapturer,VaapiVideoDecoder"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
      ];
    };
    mpv' = mpv.override {
      scripts = with mpvScripts; [
        autocrop
        inhibit-gnome
      ];
    };
  in [
    browserpass
    chromium'
    eduvpn-client
    evince
    evolution
    gimp
    gnome-frog
    gnomeExtensions.appindicator
    gnomeExtensions.bing-wallpaper-changer
    gnomeExtensions.gsconnect
    gnomeExtensions.pop-shell
    gnomeExtensions.xwayland-indicator
    gnucash
    inkscape
    libnotify
    mpv'
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
