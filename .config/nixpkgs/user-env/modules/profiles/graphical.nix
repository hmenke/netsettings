{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; [
    browserpass
    (chromium.override {
      commandLineArgs = [
        "--disk-cache-dir=/dev/shm/$USER/cache/chromium"
        "--disable-gpu-shader-disk-cache"
        "--enable-features=WebRTCPipeWireCapturer,VaapiVideoDecoder,Vulkan"
        "--enable-gpu-rasterization"
        "--enable-zero-copy"
        "--use-vulkan"
      ];
    })
    evince
    evolution
    gimp
    gnomeExtensions.appindicator
    gnomeExtensions.bing-wallpaper-changer
    gnomeExtensions.gsconnect
    gnomeExtensions.pop-shell
    libnotify
    newsflash
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xdragon
    xournalpp
  ];
}
