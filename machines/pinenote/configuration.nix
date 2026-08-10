{
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware.nix
    # ./gnome.nix
    ./sway.nix
  ];

  boot.tmp.cleanOnBoot = true;

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
    # lisgd needs raw access to the touchscreen for gestures (in particular
    # the swipe that opens the bar-ctrl side bar).
    "input"
  ];

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager.enable = true;
    hostName = "pinenote";
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services = {
    journald.storage = "volatile";
    logind.settings.Login = {
      HandlePowerKey = "suspend";
      HandlePowerKeyLongPress = "poweroff";
    };
  };

  fonts = {
    packages = with pkgs; [
      font-awesome
      nerd-fonts.symbols-only
      noto-fonts
      noto-fonts-color-emoji
      liberation_ttf
    ];
    fontDir.enable = true;
  };

  fileSystems = {
    "/" = {
      label = "nixos";
      fsType = "ext4";
    };
    "/home" = {
      label = "data";
      fsType = "ext4";
    };
  };

  system.stateVersion = "25.05";

  home-manager.users = {
    root.home.stateVersion = "25.05";
    kurnevsky = {
      programs.git.enable = true;
      home.stateVersion = "25.05";
    };
  };
}
