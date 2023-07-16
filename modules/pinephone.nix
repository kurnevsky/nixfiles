{ pkgs, lib, ... }:

let uuid = "e3b552a3-be28-4d1d-9016-e1b9a8699464";
in {
  mobile.generatedFilesystems.rootfs = lib.mkDefault {
    # image builder uses `mkfs.btrfs --rootdir` which retains file owners
    # this breaks nix daemon because /nix should be owned by root
    # type = lib.mkForce "btrfs";
    label = lib.mkForce "root";
    id = lib.mkForce uuid;
  };

  fileSystems."/" = {
    # fsType = "btrfs";
    # options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    device = "/dev/disk/by-uuid/${uuid}";
  };

  # breaks encryption
  mobile.quirks.supportsStage-0 = lib.mkForce false;

  users = {
    mutableUsers = lib.mkForce true;
    users.kurnevsky = {
      passwordFile = lib.mkForce null;
      password = "1234";
    };
  };

  mobile.beautification.splash = true;

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager = {
      enable = true;
      unmanaged = [ "rndis0" "usb0" ];
    };
  };

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = lib.mkForce false;
    sensor.iio.enable = true;
  };

  services = {
    pipewire = {
      enable = true;
      systemWide = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
    xserver = {
      enable = true;
      desktopManager.plasma5.mobile.enable = true;
      displayManager = {
        sddm.enable = true;
        defaultSession = "plasma-mobile";
      };
    };
  };
}
