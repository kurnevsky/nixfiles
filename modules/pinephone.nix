{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:

{
  imports = [ ./pinephone-plasma-mobile ];

  boot.tmp.cleanOnBoot = true;

  environment.systemPackages = with pkgs; [
    gnupg
    firefox-mobile
    telegram-desktop
    wesnoth
    megapixels
    android-tools
    usbutils
  ];

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
    # The default accesses `kernel.buildDTBs` which the mobile-nixos kernel
    # builder doesn't define.
    deviceTree.enable = lib.mkDefault pkgs.stdenv.hostPlatform.isAarch64;
  };

  home-manager.users.kurnevsky = {
    programs.git = {
      enable = true;
      signing.format = "openpgp";
    };
    services = {
      gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-qt;
      };
    };
  };
}
