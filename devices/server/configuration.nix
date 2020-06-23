{ config, pkgs, options, ... }:

let
  secrets = import "/etc/nixos/secrets/secrets.nix";
  acmeRoot = "/var/lib/acme/acme-challenge";
  fcgiwrapSocket = "/run/fcgiwrap.socket";
  emacsPkg = pkgs.emacsPackagesGen pkgs.emacs-nox;
  myEmacs = emacsPkg.emacsWithPackages (epkgs: (
    with epkgs.melpaPackages; [
      nix-mode
    ]));
  myNeoVim = pkgs.neovim.override {
    vimAlias = true;
    configure = {
      plug.plugins = with pkgs.vimPlugins; [
        vim-nix
        vim-sensible
      ];
    };
  };
  cgitrc = import ../../cgitrc.nix {
    inherit config pkgs;
  };
  gitShell = "${pkgs.git}/bin/git-shell";
in
{
  imports =
    [
      ../../modules/services/networking/shadowsocksWithPlugins.nix
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  system = {
    stateVersion = "20.03";
    autoUpgrade = {
      enable = true;
      allowReboot = true;
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
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
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
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
    "net.ipv6.conf.default.accept_ra" = 0;
    "net.ipv6.conf.default.autoconf" = 0;
    "net.ipv6.conf.all.accept_ra" = 0;
    "net.ipv6.conf.all.autoconf" = 0;
    "net.ipv6.conf.ens3.accept_ra" = 0;
    "net.ipv6.conf.ens3.autoconf" = 0;
  };

  security.apparmor.enable = true;

  networking = {
    enableIPv6 = true;
    hostName = "henrimenke.com";

    useDHCP = false;
    nameservers = [ "46.38.225.230" "46.38.252.230" "2a03:4000:0:1::e1e6" "2a03:4000:8000::fce6" ];
    defaultGateway = "94.16.116.1";
    defaultGateway6 = { address = "fe80::1"; interface = "ens3"; };
    interfaces.ens3 = {
      ipv4 = {
        addresses = [ { address = "94.16.117.117"; prefixLength = 22; } ];
      };
      ipv6 = {
        #addresses = [ { address = "2a03:4000:29:637:b8ab:57ff:fef6:37ca"; prefixLength = 64; } ];
        addresses = [ { address = "2a03:4000:29:637::1"; prefixLength = 64; } ];
      };
    };

    firewall = {
      enable = true;
      allowPing = true;
      pingLimit = "--limit 1/minute --limit-burst 5";
      rejectPackets = true;
      allowedTCPPorts = [ 80 443 8388 ];
      allowedUDPPorts = [ 1194 8388 ];
      trustedInterfaces = [ "wg0" ];
      extraCommands = ''
        iptables -t nat -A POSTROUTING -o ens3 -j MASQUERADE
        ip6tables -t nat -A POSTROUTING -o ens3 -j MASQUERADE
      '';
    };

    nat = {
      enable = true;
      externalInterface = "ens3";
      internalInterfaces = [ "wg0" ];
    };

    wireguard = {
      enable = true;
      interfaces = {
        wg0 = {
          privateKeyFile = secrets.wireguardKey;
          ips = [ "10.200.200.1/24" "fd42:42:42::1/64" ];
          listenPort = 1194;
          peers = [
            # laptop
            { publicKey = "Caij1POBl4n/g/1JzjYEGSIPu4AGrGe484xLkPrc7hY=";
              allowedIPs = [ "10.200.200.2/32" "fd42:42:42::2/128" ]; }
            # otago
            { publicKey = "OroUPwQ/detTLkTdTmj5TXLOQP+SF2RmdARZZrG7FT0=";
              allowedIPs = [ "10.200.200.3/32" "fd42:42:42::3/128" ]; }
            # Aleks
            { publicKey = "DFeyur2OmD5KAtXny2SzBIm2h9mcyCmaymhttPSGshU=";
              allowedIPs = [ "10.200.200.4/32" "fd42:42:42::4/128" ]; }
            # Aleks work laptop
            { publicKey = "b+v2qqBUHkelDChGUvIuIXeL61+v3SMhVK0mRvfRgHs=";
              allowedIPs = [ "10.200.200.5/32" "fd42:42:42::5/128" ]; }
            # worklaptop
            { publicKey = "SajAZyMIX7NQaKhcdMJqKJIM7JWqDzbmAQeAhYXJCRA=";
              allowedIPs = [ "10.200.200.6/32" "fd42:42:42::6/128" ]; }
            # Phone
            { publicKey = "/17L7mz6AwqqMQD+lBCbTN4pYt2A4IzpPOmvmneZ/H4=";
              allowedIPs = [ "10.200.200.7/32" "fd42:42:42::7/128" ]; }
          ];
        };
      };
    };
  };

  console = {
    keyMap = "us";
  };
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment = {
    variables = {
      EDITOR = "nvim";
    };

    systemPackages = with pkgs; [
      bind
      cgit
      curl
      git
      gnupg
      myEmacs
      myNeoVim
      pandoc
      tcpdump
      tmux
    ];

    shells = with pkgs; [
      gitShell
    ];
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.postfix = {
    enable = true;
    hostname = "henrimenke.com";
    origin = "henrimenke.com";
    destination = [ "localhost" "henrimenke.com" ];
    networks = [ "127.0.0.0/8" "[::ffff:127.0.0.0]/104" "[::1]/128" ];
    extraConfig = ''
      inet_interfaces = loopback-only
      authorized_submit_users = root backup journalwatch
    '';
  };

  services.journalwatch = {
    enable = true;
    mailFrom = "journalwatch@henrimenke.com";
    mailTo = "henrimenke@gmail.com";
    interval = "daily";
    priority = 6;
    filterBlocks = options.services.journalwatch.filterBlocks.default ++ [
      { filters = builtins.concatStringsSep "\n" [
          "refused connection: .*"
        ];
        match = "SYSLOG_IDENTIFIER = kernel"; }
      { filters = builtins.concatStringsSep "\n" [
          "error: PAM: Authentication failure for .*"
          "error: maximum authentication attempts exceeded for .*"
          "error: Bad remote protocol version identification: .*"
          "error: Received disconnect from .*"
          "error: kex_exchange_identification: .*"
          "Bad protocol version identification .*"
          "Connection closed by .*"
          "Connection from .*"
          "Connection reset by authenticating user .*"
          "Did not receive identification string from .*"
          "Disconnected from .*"
          "Disconnected from authenticating user .*"
          "Disconnecting authenticating user .*"
          "Disconnecting invalid user .*"
          "Failed keyboard-interactive/pam for .*"
          "Invalid user .*"
          "Received disconnect from .*"
          "Unable to negotiate with .*"
        ];
        match = "SYSLOG_IDENTIFIER = sshd"; }
      { filters = builtins.concatStringsSep "\n" [
          "Condition check resulted in .* being skipped\\."
        ];
        match = "SYSLOG_IDENTIFIER = systemd"; }
      { filters = builtins.concatStringsSep "\n" [
          ".*monitored file `[^`]*` was moved into place, adding watch.*"
          ".*monitoring (file|directory) `[^`]*`.*"
        ];
        match = "SYSLOG_IDENTIFIER = nscd"; }
    ];
  };

  services.fcgiwrap = {
    enable = true;
    socketAddress = fcgiwrapSocket;
  };

  security.acme = {
    acceptTerms = true;
    email = "henrimenke@gmail.com";
    certs = {
      "henrimenke.com" = {
        webroot = acmeRoot;
        extraDomains = {
          "www.henrimenke.com" = null;
          "git.henrimenke.com" = null;
          "git-private.henrimenke.com" = null;
        };
        postRun = "systemctl reload nginx.service";
      };
    };
  };

  services.nginx = {
    enable = true;

    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    appendHttpConfig = ''
      server_names_hash_bucket_size 64;
      error_log syslog:server=unix:/dev/log;
      types {
        text/plain org;
      }
    '';

    sslProtocols = "TLSv1.2 TLSv1.3";
    sslCiphers = "EECDH+AESGCM:EDH+AESGCM";
    recommendedTlsSettings = false;
    commonHttpConfig = ''
      # SSL parameters
      ssl_prefer_server_ciphers on;
      ssl_ecdh_curve secp384r1; # Requires nginx >= 1.1.0
      ssl_session_cache shared:le_nginx_SSL:1m;
      ssl_session_timeout 1440m;
      ssl_session_tickets off; # Requires nginx >= 1.5.9
      ssl_stapling on; # Requires nginx >= 1.3.7
      ssl_stapling_verify on; # Requires nginx => 1.3.7

      add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
      add_header X-Frame-Options DENY;
      add_header X-Content-Type-Options nosniff;
      add_header X-XSS-Protection "1; mode=block";
    '';

    preStart = let
      cfg = config.services.nginx;
    in
    options.services.nginx.preStart.default + ''
      install -dm 700 -o ${cfg.user} -g ${cfg.group} ${cfg.stateDir}/cgit/
    '' + builtins.concatStringsSep "\n" (map
      (host: "install -dm 700 -o ${cfg.user} -g ${cfg.group} ${cfg.stateDir}/cgit/${host}/")
      (builtins.attrNames cfg.virtualHosts));

    virtualHosts = {
      "henrimenke.com" = {
        forceSSL = true;
        enableACME = true;
        acmeRoot = acmeRoot;
        http2 = true;
        default = true;

        root = "/var/www/henrimenke.com/html";

        serverName = "henrimenke.com";
        serverAliases = [ "www.henrimenke.com" ];

        locations = {
          "/" = {
            index = "index.html";
            tryFiles = "$uri $uri/ =404";
          };

          "~ (Theo2|Theo2-booklet|Theo5|Theo5-booklet).pdf$" = {
            extraConfig = ''
              auth_basic secured;
              auth_basic_user_file ${pkgs.writeText "htpasswd" ''
                QM:{PLAIN}QM
              ''};
            '';
          };

          #"/QQV4h3OoOGDgAt1w1CDe/" = {
          #  proxyPass = "http://localhost:8389";
          #  proxyWebsockets = true;
          #};

          "/webdav" = {
            root = "/var/www/henrimenke.com/dav";
            extraConfig = ''
              auth_basic secured;
              auth_basic_user_file ${pkgs.writeText "htpasswd" ''
                henri:$apr1$JSa06Hu5$WLukPx8Uy5tTacvD1JVs01
              ''};
              dav_methods     PUT DELETE MKCOL COPY MOVE;
              dav_ext_methods PROPFIND OPTIONS;
              dav_access      user:rw group:rw all:r;
              autoindex       on;
            '';
          };

        };
      };

      "git.henrimenke.com" = {
        forceSSL = true;
        useACMEHost = "henrimenke.com";
        acmeRoot = acmeRoot;
        http2 = true;

        root = "${pkgs.cgit}/cgit";

        serverName = "git.henrimenke.com";

        locations = {
          "/" = {
            tryFiles = "$uri @cgit";
          };

          "@cgit" = {
            extraConfig = ''
              include ${pkgs.nginx}/conf/fastcgi_params;
              fastcgi_param SCRIPT_FILENAME ${pkgs.cgit}/cgit/cgit.cgi;
              fastcgi_param CGIT_CONFIG ${pkgs.writeTextFile {
                name = "cgitrc";
                text = ''
                  # Load common settings
                  include=${cgitrc}
                  # Scan for repos
                  scan-path=/home/git/public/
                '';
              }};
              fastcgi_param PATH_INFO $uri;
              fastcgi_param QUERY_STRING $args;
              fastcgi_param HTTP_HOST $server_name;
              fastcgi_pass unix:${fcgiwrapSocket};
            '';
          };
        };
      };

      "git-private.henrimenke.com" = {
        forceSSL = true;
        useACMEHost = "henrimenke.com";
        acmeRoot = acmeRoot;
        http2 = true;

        root = "${pkgs.cgit}/cgit";

        serverName = "git-private.henrimenke.com";

        locations = {
          "/" = {
            tryFiles = "$uri @cgit";
          };

          "@cgit" = {
            extraConfig = ''
              auth_basic secured;
              auth_basic_user_file ${pkgs.writeText "htpasswd" ''
                Sr2RuO4:{PLAIN}fitness
              ''};
              include ${pkgs.nginx}/conf/fastcgi_params;
              fastcgi_param SCRIPT_FILENAME ${pkgs.cgit}/cgit/cgit.cgi;
              fastcgi_param CGIT_CONFIG ${pkgs.writeTextFile {
                name = "cgitrc";
                text = ''
                  # Load common settings
                  include=${cgitrc}
                  # Scan for repos
                  scan-path=/home/git/protected/
                '';
              }};
              fastcgi_param PATH_INFO $uri;
              fastcgi_param QUERY_STRING $args;
              fastcgi_param HTTP_HOST $server_name;
              fastcgi_pass unix:${fcgiwrapSocket};
            '';
          };
        };
      };
    };
  };

  services.shadowsocksWithPlugins = {
    enable = true;
    encryptionMethod = "chacha20-ietf-poly1305";
    passwordFile = secrets.shadowsocksKey;
    localAddress = [ "0.0.0.0" ];
    port = 8388;
    mode = "tcp_and_udp";
    fastOpen = false;
    plugin = "${pkgs.v2ray-plugin}/bin/v2ray-plugin";
    pluginOpts = "server;host=henrimenke.com";
  };

  services.unbound = {
    enable = true;
    allowedAccess = [
      "127.0.0.1"
      "10.200.200.0/24"
    ];
    interfaces = [
      "0.0.0.0"
    ];
    enableRootTrustAnchor = true;
    extraConfig = builtins.readFile ../../unbound.conf;
  };

  systemd.services.webdav = {
    description = "WebDAV auto-commit";
    path = with pkgs; [ git ];
    script = ''
      git add webdav
      if ! git diff-index --quiet HEAD; then
        git commit -am "WebDAV auto-commit $(date +'%FT%T')"
      fi
    '';
    serviceConfig = {
      Type = "oneshot";
      User = config.services.nginx.user;
      Group = config.services.nginx.group;
      WorkingDirectory = "/var/www/henrimenke.com/dav/";
    };
  };

  systemd.paths.webdav = {
    description = "WebDAV auto-commit";
    wantedBy = [ "nginx.service" ];
    pathConfig = {
      DirectoryNotEmpty = "/var/www/henrimenke.com/dav/webdav/";
      Unit = "webdav.service";
    };
  };

  users.mutableUsers = false;
  users.users = {
    root = {
      passwordFile = secrets.root.passwordFile;
      openssh.authorizedKeys.keyFiles = secrets.root.authorizedKeys;
    };
    git = {
      isNormalUser = true;
      shell = gitShell;
      openssh.authorizedKeys.keyFiles = secrets.git.authorizedKeys;
    };
    henri = {
      uid = 1000;
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      passwordFile = secrets.henri.passwordFile;
      openssh.authorizedKeys.keyFiles = secrets.henri.authorizedKeys;
    };
  };
}
