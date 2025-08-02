{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
    tmp.cleanOnBoot = true;
  };

  system.stateVersion = "25.05";

  home-manager.users = {
    root.home.stateVersion = "25.05";
    kurnevsky.home.stateVersion = "25.05";
  };
}
