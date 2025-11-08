{ pkgs, lib, ... }:

{
  boot.tmp.cleanOnBoot = true;

  environment = {
    systemPackages =
      with pkgs;
      [
        (pass-wayland.withExtensions (
          ext: with ext; [
            pass-otp
            pass-update
          ]
        ))
        gnupg
        firefox-mobile
        telegram-desktop
        wesnoth
        megapixels
      ]
      ++ (with pkgs.kdePackages; [
        qmlkonsole
        okular
      ]);
    plasma5.excludePackages = with pkgs.kdePackages; [ konsole ];
  };

  i18n.supportedLocales = [
    "C.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "ru_RU.UTF-8/UTF-8"
  ];

  users.users.kurnevsky.extraGroups = [
    "dialout"
    "networkmanager"
    "video"
    "pipewire"
    "audio"
  ];

  mobile = {
    beautification.splash = true;
    # Setup USB gadget networking in initrd.
    boot.stage-1.networking.enable = lib.mkDefault true;
  };

  zramSwap.enable = true;

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager = {
      enable = true;
      # Ensures any rndis config from stage-1 is not clobbered by NetworkManager.
      unmanaged = [
        "rndis0"
        "usb0"
      ];
    };
  };

  security.rtkit.enable = true;

  hardware = {
    bluetooth.enable = true;
    sensor.iio.enable = true;
  };

  services.xserver = {
    enable = true;
    desktopManager.plasma5.mobile.enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        extraSeatDefaults = ''
          session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
        '';
      };
      defaultSession = "plasma-mobile";
      autoLogin = {
        enable = true;
        user = "kurnevsky";
      };
    };
  };

  home-manager.users.kurnevsky = {
    programs.git.enable = true;
    services = {
      gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-qt;
      };
    };
  };
}
