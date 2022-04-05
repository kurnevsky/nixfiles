{ pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl."kernel.sysrq" = 1;
    kernelPatches = [{
      name = "nouveau";
      patch = ./nouveau.patch;
    }];
    loader.grub = {
      enable = true;
      device = "/dev/sda";
    };
    initrd.kernelModules = [ "nouveau" ];
  };

  networking = {
    hostName = "acer";
    useDHCP = false;
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
    };
    wireguard.interfaces.wg0 = {
      listenPort = 51871;
      privateKeyFile = "/secrets/wg/private.key";
      peers = [{
        endpoint = "kurnevsky.net:51871";
        publicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
        presharedKeyFile = "/secrets/wg/preshared.psk";
        allowedIPs = [ "192.168.14.0/24" ];
        persistentKeepalive = 25;
        dynamicEndpointRefreshSeconds = 30;
      }];
      ips = [ "192.168.14.4/32" ];
    };
  };

  console = {
    font = "cyr-sun16";
    keyMap = "ru";
  };

  time.timeZone = "Europe/Minsk";

  environment.systemPackages = with pkgs; [
    mc
    firefox-wayland
    vlc
    qtox
    tdesktop
    xorg.xmodmap
  ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
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
      config = {
        Peers = [
          "tls://yggno.de:18227"
          "tls://box.paulll.cc:13338"
          "tls://54.37.137.221:11129"
          "tls://pl1.servers.devices.cwinfo.net:11129"
          "tls://193.111.114.28:1443"
          "tls://ygg-ukr.incognet.io:8884"
        ];
        AllowedPublicKeys = [
          "cb875a43a4afadb2a5fa3ec785f5f041d5a7c8c3533f9a245634329294d4a558"
          "e251c33f1776870ec2606f380c0ad79109f6be440fe52b240757c37854711ef1"
        ];
      };
      openMulticastPort = true;
      persistentKeys = true;
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
      desktopManager.plasma5.enable = true;
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
  };

  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };
    opengl.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  users.users = {
    parents = {
      uid = 1001;
      isNormalUser = true;
      shell = pkgs.zsh;
      passwordFile = "/secrets/parents";
      extraGroups = [ "audio" "video" ];
    };
  };

  systemd.services = {
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
    "wireguard-wg0-peer-5JHCxIYeZ50k7YJM\\x2bkLAbqGW4LAXpI5lycYEWSVxkBE\\x3d-refresh".serviceConfig =
      {
        Restart = "always";
        RestartSec = "30";
      };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    parents = {
      home = {
        stateVersion = "21.11";
        file.".Xmodmap".text = ''
          keycode 191 = dollar
          keycode 192 = EuroSign
        '';
      };
      systemd.user.services.x0vncserver = {
        Unit.Description = "Remote desktop service (VNC)";
        Service = {
          Type = "simple";
          # wait for Xorg started by ${USER}
          ExecStartPre =
            "${pkgs.bash}/bin/sh -c 'while ! ${pkgs.procps}/bin/pgrep -U \"$USER\" plasmashell; do ${pkgs.coreutils}/bin/sleep 2; done'";
          ExecStart =
            "${pkgs.tigervnc}/bin/x0vncserver -rfbauth /home/\${USER}/.vnc/passwd";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
    kurnevsky.home.stateVersion = "21.11";
  };
}
