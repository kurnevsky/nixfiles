{ config, pkgs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.luks.devices.root.allowDiscards = true;
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.extraModulePackages = with config.boot.kernelPackages; [
    acpi_call
    v4l2loopback
  ];

  fileSystems."/".options = [ "noatime" "nodiratime" "compress=zstd:3" ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
