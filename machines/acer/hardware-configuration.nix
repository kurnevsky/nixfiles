{
  config,
  lib,
  modulesPath,
  ...
}:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd = {
      availableKernelModules = [
        "uhci_hcd"
        "ehci_pci"
        "ahci"
        "usb_storage"
        "sd_mod"
        "sr_mod"
      ];
      kernelModules = [ "dm-snapshot" ];
    };
    kernelModules = [ ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/8458f8eb-7af5-4ff4-934a-d8d8a886fe92";
      fsType = "ext4";
    };
    "/home" = {
      device = "/dev/disk/by-uuid/8290087a-956b-41db-a004-a4df1a6ce930";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/e86db406-3793-400d-bbef-d42654f5608d";
      fsType = "ext4";
    };
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/e15e4a3e-c561-4511-83a0-db50009d2349"; } ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
