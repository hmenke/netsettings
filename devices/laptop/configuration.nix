{ config, pkgs, lib, ... }:

let
  secrets = import "/etc/nixos/secrets/secrets.nix";
in
{
  imports =
    [
      ../../modules/system/boot/loader/systemd-boot/systemd-boot.nix
      ./hardware-configuration.nix
    ];

  # Hardware specific stuff
  hardware = {
    acpilight.enable = true;
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
  };
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ "i915.enable_rc6=7" ];
    kernelModules = [ "acpi_call" "i915" "tpm-rng" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    loader = {
      systemd-boot = {
        enable = true;
        signed = true;
        signing-key = "/persist/etc/efi-keys/db.key";
        signing-certificate = "/persist/etc/efi-keys/db.crt";
      };
      efi.canTouchEfiVariables = true;
    };
  };

  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "cryptd"
  ];
  boot.initrd.luks.devices.cryptlvm = {
    device = "/dev/disk/by-uuid/91f97e4b-180d-45fc-b1d6-772353f068bf";
    allowDiscards = true;
  };
  #boot.initrd.extraUtilsCommands = lib.mkAfter ''
  #  copy_bin_and_libs ${pkgs.utillinux}/bin/wipefs
  #  copy_bin_and_libs ${pkgs.e2fsprogs}/bin/mke2fs
  #'';
  #boot.initrd.postDeviceCommands = lib.mkAfter ''
  #  wipefs -a /dev/mapper/cryptlvm-root
  #  mke2fs -t ext4 -L root /dev/mapper/cryptlvm-root
  #'';
  #fileSystems."/" = lib.mkForce {
  #  device = "/dev/disk/by-label/root";
  #  fsType = "ext4";
  #};
  fileSystems."/persist" = {
    neededForBoot = true;
  };

  system = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    stateVersion = "20.09"; # Did you read the comment?
    autoUpgrade = {
      enable = true;
      allowReboot = false;
    };
  };

  nix = {
    optimise = {
      automatic = true;
      dates = [ "daily" ];
    };
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 10d";
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = [
      (import ../../packages/overlay.nix)
    ];
  };

  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
  };

  security = {
    apparmor.enable = true;
  };

  networking = {
     hostName = "laptop";
     useDHCP = false;
     interfaces.wlp3s0.useDHCP = false;
     networkmanager = {
       enable = true;
     };
  };

  console = {
    keyMap = "us";
  };
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Pacific/Auckland";

  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.de
    aspellDicts.en
    emacs
    ffmpeg
    fzf
    gdb
    git
    gnuplot
    jq
    lua
    msmtp
    neomutt
    neovim
    newsboat
    notmuch
    offlineimap
    openssl
    p7zip
    pandoc
    parted
    pass-otp
    picom
    polybarFull
    progress
    proxychains
    python3
    qpdf
    rcs
    ripgrep
    rsync
    shadowsocks-libev
    shellcheck
    sshuttle
    tree
    ts
    unison
    unzip
    valgrind
    v2ray-plugin
    wget
    which
    whois
    wireguard-tools
    youtube-dl
    zip
  ] ++ [
    arandr
    clementine
    dunst
    evince
    feh
    firefox
    gimp
    libnotify
    networkmanagerapplet
    pavucontrol
    polybar
    qemu
    redshift
    rofi
    scrcpy
    scrot
    thunderbird
    vlc
    xclip
    xorg.setxkbmap
    xorg.xbacklight
    xorg.xmodmap
    xorg.xsetroot
    xscreensaver
    zathura
  ] ++ [
    dropbox
    google-chrome
    mathematica11
    skypeforlinux
    softmaker-office
    zoom-us
  ];

  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      noto-fonts
      noto-fonts-extra
      noto-fonts-emoji
      noto-fonts-cjk
      vegur # the official NixOS font
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };
    adb.enable = true;
    firejail.enable = true;
    tmux.enable = true;
  };

  services.openssh = {
    enable = true;
    hostKeys = [
      { path = "/persist/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519"; }
      { path = "/persist/etc/ssh/ssh_host_rsa_key";
        type = "rsa";
        bits = 4096; }
    ];
  };

  sound.enable = true;
  hardware = {
    u2f.enable = true;
    pulseaudio.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";
    videoDrivers = [ "intel" ];
    deviceSection = lib.mkDefault ''
      Option "TearFree" "true"
      Option "AccelMethod" "sna"
      Option "Backlight" "intel_backlight"
    '';

    # Enable touchpad support.
    libinput.enable = true;

    # Enable the Gnome Desktop Environment.
    displayManager.gdm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "henri";
      };
    };
    windowManager.bspwm = {
      enable = true;
    };
  };

  services.tlp.enable = true;
  services.gvfs.enable = true;

  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [
    libu2f-host
    yubikey-personalization
  ];

  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults rootpw
      Defaults tty_tickets
      Defaults lecture="never"
    '';
  };

  users.mutableUsers = false;
  users.users = {
    root = {
      passwordFile = secrets.henri.passwordFile;
      openssh.authorizedKeys.keyFiles = secrets.henri.authorizedKeys;
    };
    henri = {
      uid = 1000;
      isNormalUser = true;
      shell = "/run/current-system/sw/bin/zsh";
      extraGroups = [ "adbusers" "networkmanager" "pcscd" "video" "wheel" ];
      passwordFile = secrets.root.passwordFile;
      openssh.authorizedKeys.keyFiles = secrets.root.authorizedKeys;
    };
  };
}

