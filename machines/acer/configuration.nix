{ pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
    kernelPackages = pkgs.linuxPackages_xanmod;
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
        sessionCommands = let
          layout = pkgs.writeText "xkb-layout" ''
            keycode 183 = dollar
            keycode 184 = EuroSign
          '';
        in "${pkgs.xorg.xmodmap}/bin/xmodmap ${layout}";
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
      home.stateVersion = "21.11";
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