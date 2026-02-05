{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    chromium' = chromium.override {
      commandLineArgs = [
        "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
        "--disable-gpu-shader-disk-cache"
        "--disable-features=ExtensionManifestV2Unsupported,ExtensionManifestV2Disabled"
        "--enable-features=WebRTCPipeWireCapturer,VaapiVideoDecoder"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
      ];
    };
    mpv' = mpv.override {
      scripts = with mpvScripts; [
        inhibit-gnome
      ];
    };
  in [
    browserpass
    chromium'
    dragon-drop
    eduvpn-client
    evince
    evolution
    gimp
    gnome-frog
    gnomeExtensions.appindicator
    gnomeExtensions.bing-wallpaper-changer
    gnomeExtensions.gsconnect
    gnomeExtensions.launch-new-instance
    gnomeExtensions.pop-shell
    gnomeExtensions.removable-drive-menu
    gnomeExtensions.vitals
    gnomeExtensions.xwayland-indicator
    gnucash
    inkscape
    libgtop
    libnotify
    lm_sensors
    mousai
    mpv'
    mupdf
    songrec
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xournalpp
  ];
}
