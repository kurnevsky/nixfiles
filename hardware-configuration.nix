# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ ];

  boot.initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/mapper/root";
      fsType = "btrfs";
    };

  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/67f53f6d-177a-4caf-b16d-2db8ddb7674c";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/54A5-B68D";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 1;
  virtualisation.virtualbox.guest.enable = true;
}