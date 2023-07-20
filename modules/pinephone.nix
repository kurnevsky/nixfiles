{ pkgs, lib, ... }:

{
  environment = {
    systemPackages = with pkgs;
      [ gnupg firefox-mobile telegram-desktop wesnoth megapixels ]
      ++ (with pkgs.plasma5Packages; [ index qmlkonsole okular ]);
    plasma5.excludePackages = with pkgs.plasma5Packages; [ konsole ];
  };

  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" ];

  users.users.kurnevsky.extraGroups =
    [ "dialout" "networkmanager" "video" "pipewire" "audio" ];

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
      unmanaged = [ "rndis0" "usb0" ];
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

  # TODO: dedup with desktop
  home-manager.users.kurnevsky = {
    programs = {
      git = {
        enable = true;
        userName = "Evgeny Kurnevsky";
        userEmail = "kurnevsky@gmail.com";
        signing = {
          key = null;
          signByDefault = true;
        };
        aliases = {
          lol = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
        };
        delta.enable = true;
        lfs.enable = true;
        extraConfig = {
          push.default = "simple";
          merge.conflictstyle = "diff3";
          pull.ff = "only";
          safe.directory = "/etc/nixos";
          github.user = "kurnevsky";
          gitlab.user = "kurnevsky";
          gitlab."gitlab.evolution.com/api/v4".user = "ykurneuski";
        };
      };
    };
    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        sshKeys = [ "53D3B2AAF43FA184A31ACEC71295A713D5B9A123" ];
        defaultCacheTtl = 14400;
        maxCacheTtl = 14400;
        defaultCacheTtlSsh = 14400;
        maxCacheTtlSsh = 14400;
        pinentryFlavor = "qt";
        extraConfig = "allow-loopback-pinentry";
      };
    };
  };
}
