{ pkgs, config, ... }:

{
  boot = {
    tmp.cleanOnBoot = true;
    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl."kernel.sysrq" = 1;
    kernelPatches = [
      {
        name = "nouveau";
        patch = ./nouveau.patch;
      }
    ];
    loader.grub = {
      enable = true;
      device = "/dev/sda";
    };
    initrd.kernelModules = [ "nouveau" ];
  };

  networking = {
    hostName = "acer";
    useDHCP = false;
    useNetworkd = true;
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # VNC
        5900
      ];
      allowedUDPPorts = [
        # WireGuard
        51871
      ];
      trustedInterfaces = [
        "wg0"
        "icmp"
        "dns0"
      ];
    };
    wireguard.enable = true;
  };

  console = {
    font = "cyr-sun16";
    keyMap = "ru";
  };

  i18n = {
    supportedLocales = [
      "C.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
      "ru_RU.UTF-8/UTF-8"
    ];
    defaultLocale = "ru_RU.UTF-8";
  };

  time.timeZone = "Europe/Minsk";

  environment.systemPackages = with pkgs; [
    mc
    firefox
    ungoogled-chromium
    ffmpeg-full
    mpv
    vlc
    qtox
    telegram-desktop
    xorg.xmodmap
  ];

  fonts.packages = with pkgs; [
    nerd-fonts.hack
    noto-fonts
    noto-fonts-color-emoji
    noto-fonts-monochrome-emoji
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    symbola
  ];

  gtk.iconCache.enable = true;

  services = {
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTRS{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -B 254 /dev/%k"
    '';
    avahi = {
      enable = true;
      denyInterfaces = [ "tun0" ];
    };
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
    yggdrasil = {
      enable = true;
      settings = {
        Peers = [
          "tls://yggno.de:18227"
          "tls://box.paulll.cc:13338"
          "tls://ygg.mkg20001.io:443"
        ];
        AllowedPublicKeys = [
          "9734b6eccb555f3791e6229c741c1355b05d5741c82ad16d58d5e4085b8d3ee3"
          "e251c33f1776870ec2606f380c0ad79109f6be440fe52b240757c37854711ef1"
        ];
      };
      openMulticastPort = true;
      persistentKeys = true;
    };
    hans.clients.vps = {
      server = "kropki.org";
      passwordFile = config.age.secrets.hans.path or "/secrets/hans";
      extraConfig = "-d icmp -m 1200";
    };
    iodine.clients.vps = {
      server = "i.kropki.org";
      passwordFile = config.age.secrets.iodine.path or "/secrets/iodine";
    };
    desktopManager.plasma6.enable = true;
    displayManager = {
      defaultSession = "plasmax11";
      autoLogin = {
        enable = true;
        user = "parents";
      };
      sddm = {
        enable = true;
        autoNumlock = true;
        settings.Users.HideUsers = "kurnevsky";
      };
    };
    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
    };
    xserver = {
      enable = true;
      videoDrivers = [ "modesetting" ];
    };
  };

  security = {
    # Enable pam_systemd module to set dbus environment variable.
    pam.services.login.startSession = true;
    rtkit.enable = true;
    sudo.extraRules = [
      {
        runAs = "root";
        users = [ "parents" ];
        commands = [
          {
            command = "/run/current-system/sw/bin/systemctl start iodine-vps.service";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/systemctl start hans-vps.service";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };

  hardware = {
    bluetooth.enable = true;
    graphics.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  users = {
    users = {
      parents = {
        uid = 1001;
        isNormalUser = true;
        shell = pkgs.zsh;
        hashedPasswordFile = config.age.secrets.parents.path or "/secrets/parents";
        extraGroups = [
          "audio"
          "video"
        ];
      };
      hans.group = "hans";
    };
    groups.hans = { };
  };

  systemd = {
    services = {
      iodine-vps.wantedBy = pkgs.lib.mkForce [ ];
      hans-vps.wantedBy = pkgs.lib.mkForce [ ];
      eurodollar = {
        description = "Setkeycodes for € and $ keys";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = [
            "${pkgs.kbd}/bin/setkeycodes b3 183"
            "${pkgs.kbd}/bin/setkeycodes b4 184"
          ];
        };
      };
    };
    network = {
      enable = true;
      netdevs = {
        "99-wg0" = {
          netdevConfig = {
            Name = "wg0";
            Kind = "wireguard";
            Description = "WireGuard tunnel wg0";
          };
          wireguardConfig = {
            PrivateKeyFile = config.age.secrets.wg-private.path or "/secrets/wg/private.key";
            ListenPort = 51871;
          };
          wireguardPeers = [
            {
              PublicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
              PresharedKeyFile = config.age.secrets.wg-preshared.path or "/secrets/wg/preshared.psk";
              AllowedIPs = "192.168.14.0/24";
              # Direct connection
              # Endpoint = "kropki.org:51871";
              # Websocat connection
              # Endpoint = "127.0.0.1:42930";
              # Shadowsocks connection
              Endpoint = "127.0.0.1:51870";
              PersistentKeepalive = 25;
            }
          ];
        };
      };
      networks."99-wg0" = {
        name = "wg0";
        address = [ "192.168.14.4/32" ];
        routes = [
          {
            Destination = "192.168.14.0/24";
            Scope = "link";
          }
        ];
      };
    };
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-acer.age;
    parents.file = ../../secrets/parents-acer.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-acer.age;
    hans = {
      file = ../../secrets/hans.age;
      owner = "hans";
      group = "hans";
    };
    iodine = {
      file = ../../secrets/iodine.age;
      owner = "iodined";
      group = "iodined";
    };
    shadowsocks = {
      file = ../../secrets/shadowsocks.age;
      mode = "440";
      group = "secrets-shadowsocks";
    };
    wg-private = {
      file = ../../secrets/wg-private-acer.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
    wg-preshared = {
      file = ../../secrets/wg-preshared-acer.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    parents = {
      home = {
        stateVersion = "21.11";
        file.".Xmodmap".text = ''
          keycode 191 = EuroSign
          keycode 192 = dollar
        '';
      };
      systemd.user.services.x0vncserver = {
        Unit.Description = "Remote desktop service (VNC)";
        Service = {
          Type = "simple";
          # wait for Xorg started by ${USER}
          ExecStartPre = "${pkgs.bash}/bin/sh -c 'while ! ${pkgs.procps}/bin/pgrep -U \"$USER\" plasmashell; do ${pkgs.coreutils}/bin/sleep 2; done'";
          ExecStart = "${pkgs.tigervnc}/bin/x0vncserver -AcceptSetDesktopSize=0 -rfbauth /home/\${USER}/.vnc/passwd";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
    kurnevsky.home.stateVersion = "21.11";
  };
}
