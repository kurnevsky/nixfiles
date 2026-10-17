{ pkgs, ... }:

{
  # The phosh module enables gnome core-os-services which defaults
  # i18n.inputMethod to ibus, and GTK_IM_MODULE=ibus prevents the on-screen
  # keyboard from auto-activating in GTK apps.
  # See https://github.com/NixOS/nixpkgs/issues/538381
  i18n.inputMethod.enable = false;

  services = {
    displayManager.autoLogin = {
      enable = true;
      user = "kurnevsky";
    };
    desktopManager.phosh = {
      enable = true;
      user = "kurnevsky";
      group = "users";
    };
    xserver = {
      enable = true;
      displayManager.lightdm = {
        enable = true;
        extraSeatDefaults = ''
          session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
        '';
      };
    };
  };

  home-manager.users.kurnevsky.dconf.settings."mobi/phosh/osk".ignore-hw-keyboards = true;
}
