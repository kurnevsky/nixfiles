{ pkgs, lib, ... }:

{
  users = {
    mutableUsers = lib.mkForce true;
    users.kurnevsky = {
      passwordFile = lib.mkForce null;
      password = "1234";
    };
  };

  services.xserver = {
    enable = true;
    desktopManager.plasma5.mobile.enable = true;
    displayManager = {
      sddm.enable = true;
      defaultSession = "plasma-mobile";
    };
  };
}
