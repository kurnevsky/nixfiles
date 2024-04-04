{ modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd.availableKernelModules =
      [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4745495c-bb3a-4301-aac0-c4ec098564d5";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };
    "/home" = {
      device = "/dev/disk/by-uuid/4745495c-bb3a-4301-aac0-c4ec098564d5";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/22C0-5F93";
      fsType = "vfat";
      options = [ "umask=0077" ];
    };
  };

  boot.initrd.luks.devices."root".device =
    "/dev/disk/by-uuid/b0769eed-4542-4be8-a865-7f16614f2974";

  swapDevices = [ ];
}
