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
    discord
    element-desktop
    easyeffects
    evince
    evolution
    gajim
    gimp
    gnomeExtensions.appindicator
    gnomeExtensions.gsconnect
    gnomeExtensions.sound-output-device-chooser
    libnotify
    onlyoffice-bin
    pop-os-shell
    signal-desktop
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xournalpp
  ];
}
