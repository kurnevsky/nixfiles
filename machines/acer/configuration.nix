{ pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
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

  time.timeZone = "Europe/Minsk";

  environment.systemPackages = with pkgs; [
    mc
    firefox
    vlc
    qtox
    tdesktop
    xorg.xmodmap
  ];

  fonts.fonts = with pkgs; [
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
          "tls://54.37.137.221:11129"
          "tls://pl1.servers.devices.cwinfo.net:11129"
          "tls://193.111.114.28:1443"
          "tls://ygg-ukr.incognet.io:8884"
        ];
        AllowedPublicKeys = [
          "9734b6eccb555f3791e6229c741c1355b05d5741c82ad16d58d5e4085b8d3ee3"
          "e251c33f1776870ec2606f380c0ad79109f6be440fe52b240757c37854711ef1"
        ];
      };
      openMulticastPort = true;
      persistentKeys = true;
    };
    hans.clients.digitalocean = {
      server = "kurnevsky.net";
      passwordFile = "/secrets/hans";
      extraConfig = "-d icmp -m 1200";
    };
    iodine.clients.digitalocean = {
      server = "i.kurnevsky.net";
      passwordFile = "/secrets/iodine";
    };
    xserver = {
      enable = true;
      videoDrivers = [ "modesetting" ];
      displayManager = {
        defaultSession = "plasma";
        autoLogin = {
          enable = true;
          user = "parents";
        };
        sddm = {
          enable = true;
          autoNumlock = true;
        };
      };
      desktopManager.plasma6.enable = true;
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
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
            command = "/run/current-system/sw/bin/systemctl start iodine-digitalocean.service";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/systemctl start hans-digitalocean.service";
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
        passwordFile = "/secrets/parents";
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
      iodine-digitalocean.wantedBy = pkgs.lib.mkForce [ ];
      hans-digitalocean.wantedBy = pkgs.lib.mkForce [ ];
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
            PrivateKeyFile = "/secrets/wg/private.key";
            ListenPort = 51871;
          };
          wireguardPeers = [
            {
              PublicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
              PresharedKeyFile = "/secrets/wg/preshared.psk";
              AllowedIPs = "192.168.14.0/24";
              Endpoint = "kurnevsky.net:51871";
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
          ExecStart = "${pkgs.tigervnc}/bin/x0vncserver -rfbauth /home/\${USER}/.vnc/passwd";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
    kurnevsky.home.stateVersion = "21.11";
  };
}
