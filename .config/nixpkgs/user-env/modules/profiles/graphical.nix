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
    clementine
    discord
    element-desktop
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
    pulseeffects-pw
    signal-desktop
    virt-manager
    vlc
    wl-clipboard
    wofi
    xournalpp
  ];
}
