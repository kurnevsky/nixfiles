{ pkgs, lib, ... }:

let
  uuid = "e3b552a3-be28-4d1d-9016-e1b9a8699464";
  label = "root";
in {
  mobile.generatedFilesystems.rootfs = lib.mkDefault {
    type = lib.mkForce "btrfs";
    label = lib.mkForce label;
    id = lib.mkForce uuid;
  };

  fileSystems."/" = {
    fsType = "btrfs";
    options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    device = "/dev/disk/by-uuid/${uuid}";
  };

  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
  };
}
