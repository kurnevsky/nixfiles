# Parts of the postmarketOS UI setup that are shared between Phosh and Plasma Mobile.
{
  lib,
  pkgs,
  ...
}:

let
  hkdm = pkgs.callPackage ./packages/hkdm.nix { };
  ttyescape = pkgs.callPackage ./packages/ttyescape.nix { };
  vvmd = pkgs.callPackage ./packages/vvmd.nix { };
  vvmplayer = pkgs.callPackage ./packages/vvmplayer.nix { };
in
{
  services = {
    # pmOS uses tuned-ppd instead of power-profiles-daemon.
    power-profiles-daemon.enable = false;
    tuned = {
      enable = true;
      ppdSupport = true;
    };

    printing.enable = true;
    flatpak.enable = true;
  };

  # Required by neochat and itinerary (libquotient) with Plasma Mobile, and by
  # chatty (libcmatrix) with Phosh.
  nixpkgs.config.permittedInsecurePackages = [ "olm-3.2.16" ];

  networking.modemmanager.enable = true;

  programs.feedbackd.enable = true;

  users.users.kurnevsky.extraGroups = [ "feedbackd" ];

  environment.systemPackages = [
    ttyescape
    vvmplayer
  ];

  environment.etc."hkdm/config.d/ttyescape.toml".source =
    "${ttyescape}/etc/hkdm/config.d/ttyescape.toml";

  # Needed by buffyboard from ttyescape.
  boot.kernelModules = [ "uinput" ];

  systemd = {
    packages = [ vvmd ];
    user.services.vvmd.wantedBy = [ "default.target" ];

    services.hkdm = {
      description = "Hotkey Daemon for Mobile";
      documentation = [ "https://gitlab.postmarketos.org/postmarketOS/hkdm" ];
      wantedBy = [ "multi-user.target" ];
      restartTriggers = [ ttyescape ];
      serviceConfig = {
        ExecStart = lib.getExe hkdm;
        Restart = "always";
        RestartSec = "5s";
      };
    };
  };
}
