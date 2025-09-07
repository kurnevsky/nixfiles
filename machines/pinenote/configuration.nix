{
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware.nix
    ./gnome.nix
    # ./sway.nix
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

  # fonts = {
  #   packages = with pkgs; [
  #     comic-mono
  #   ];
  #   fontconfig.enable = true;
  #   fontconfig.defaultFonts.monospace = [ "Comic Mono" ];
  # };

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
