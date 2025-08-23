{
  pkgs,
  ...
}:

{
  boot.tmp.cleanOnBoot = true;

  i18n.supportedLocales = [
    "C.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "ru_RU.UTF-8/UTF-8"
  ];

  pinenote = {
    config.enable = true;
    pinenote-service.sway.enable = true;
  };

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

  hardware.bluetooth.enable = true;

  services = {
    journald.storage = "volatile";
    logind.extraConfig = ''
      HandlePowerKey=suspend
      HandlePowerKeyLongPress=poweroff
    '';
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
