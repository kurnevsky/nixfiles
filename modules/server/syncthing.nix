{
  pkgs,
  lib,
  ...
}:

{
  users.users.kurnevsky.linger = true;

  home-manager.users.kurnevsky.services.syncthing.settings = {
    folders."/home/kurnevsky/Sync".versioning = {
      type = "simple";
      cleanupIntervalS = 86400;
      params = {
        params.cleanoutDays = "32";
        keep = "64";
      };
    };
    options = {
      localAnnounceEnabled = pkgs.lib.mkForce false;
      localAnnouncePort = pkgs.lib.mkForce null;
    };
  };
}
