{ pkgs, ... }:

{
  networking.hostName = "pinephone";

  services.udev.extraRules = ''
    SUBSYSTEM=="leds", ACTION=="add", RUN+="${pkgs.coreutils}/bin/chgrp -R users /sys%p", RUN+="${pkgs.coreutils}/bin/chmod -R g=u /sys%p"
  '';

  system.stateVersion = "23.05";

  home-manager.users = {
    root.home.stateVersion = "23.05";
    kurnevsky.home.stateVersion = "23.05";
  };
}
