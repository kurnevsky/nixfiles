{ config, lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd = {
      availableKernelModules =
        [ "vmd" "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
      kernelModules = [ ];
      luks.devices = {
        root.device = "/dev/disk/by-uuid/ff379d89-c2c8-4324-9310-3a7150e312ae";
        data.device = "/dev/disk/by-uuid/7aca7b7d-c44f-4944-8c83-62d9de9c9aaa";
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4f507c95-5a17-4004-8451-2c1ce0a7e836";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };
    "/home" = {
      device = "/dev/disk/by-uuid/4f507c95-5a17-4004-8451-2c1ce0a7e836";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/D7D5-8A08";
      fsType = "vfat";
      options = [ "umask=0077" ];
    };
    "/home/kurnevsky/data" = {
      device = "/dev/disk/by-uuid/4f9640ad-6ab5-48c3-b0cd-9da35665a459";
      fsType = "btrfs";
      options = [ "subvol=data" ];
    };
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno2.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
