{ config, pkgs, ... }:

{
  system.fsPackages = [ pkgs.bindfs ];

  fileSystems."/var/lib/grafana/gb" = {
    device = "/home/kurnevsky/Sync/gb";
    fsType = "fuse.bindfs";

    options = [
      "nofail"
      "force-user=grafana"
      "force-group=grafana"
    ];
  };
}
