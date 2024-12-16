{ lib, ... }:

let
  uuid = "e3b552a3-be28-4d1d-9016-e1b9a8699464";
in
{
  mobile.generatedFilesystems.rootfs = lib.mkDefault {
    # image builder uses `mkfs.btrfs --rootdir` which retains file owners
    # this breaks nix daemon because /nix should be owned by root
    # type = lib.mkForce "btrfs";
    label = lib.mkForce "root";
    ext4.partitionID = lib.mkForce uuid;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/${uuid}";
    fsType = "ext4";
    # fsType = "btrfs";
    # options = [ "noatime" "nodiratime" "compress=zstd:3" ];
  };

  # mobile.boot.stage-1.kernel.additionalModules = [ "btrfs" ];

  users.users.kurnevsky = {
    hashedPasswordFile = lib.mkForce null;
    password = "1234";
  };

  system.stateVersion = "23.05";

  home-manager.users = {
    root.home.stateVersion = "23.05";
    kurnevsky.home.stateVersion = "23.05";
  };
}
