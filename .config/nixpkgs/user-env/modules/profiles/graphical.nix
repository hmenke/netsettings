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
        "--ozone-platform-hint=auto"
      ];
    })
    element-desktop
    evince
    evolution
    gimp
    gnomeExtensions.appindicator
    gnomeExtensions.gsconnect
    gnomeExtensions.pop-shell
    gnomeExtensions.sound-output-device-chooser
    libnotify
    onlyoffice-bin
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xournalpp
  ];
}
