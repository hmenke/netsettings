{ config, lib, pkgs, ... }:

{
  userPackages = with pkgs; let
    mpv' = mpv.override {
      scripts = with mpvScripts; [
        inhibit-gnome
      ];
    };
  in [
    browserpass
    dragon-drop
    eduvpn-client
    evince
    evolution
    firefox
    gimp
    gnome-frog
    gnomeExtensions.appindicator
    gnomeExtensions.bing-wallpaper-changer
    gnomeExtensions.bluetooth-battery-meter
    gnomeExtensions.gsconnect
    gnomeExtensions.launch-new-instance
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
    pop-shell
    songrec
    strawberry
    virt-manager
    vlc
    wl-clipboard
    wofi
    xournalpp
  ];
}
