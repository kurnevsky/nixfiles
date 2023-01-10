{ pkgs, ... }:

{
  services.xserver = {
    enable = true;
    desktopManager.plasma5.mobile.enable = true;
  };
}
